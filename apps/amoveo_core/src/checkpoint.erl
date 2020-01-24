-module(checkpoint).
-behaviour(gen_server).
-export([backup_p/1,
         make/0, %check if the top header with a block needs a checkpoint, and if so, makes it.
         recent/0, %returns a list of recent block hashes where the block is a checkpoint.
         clean/0, %deletes old unneeded checkpoints.
         chunk_name/1,
         sync/2,
         test/0,
         start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

%Eventually we will need a function that can recombine the chunks, and unencode the gzipped tar to restore from a checkpoint.


-include("records.hrl").

-define(LOC, constants:checkpoints()).
-record(d, {
          checkpoint_hashes = []
         }).
    %{checkpoint_depth, 100},
%-define(MFT, 1000).%max fork tolerance
%ideally this should be a configuration variable that defaults as like, 3x higher than the fork_tolerance value.

mft() ->
    case application:get_env(amoveo_core, checkpoint_depth) of
        {ok, M} -> M;
        _ -> -100%prevents checkpoints from being made.
    end.


init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if
             X == "" ->
                 #d{};
             true -> X
         end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("checkpoints died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(clean, X) -> 
    %if there are old unneeded checkpoints, delete them.
    CH2 = clean_helper(X#d.checkpoint_hashes),
    X2 = X#d{checkpoint_hashes = CH2},
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call(make, _, X) -> 
    Header = headers:top_with_block(),
    B = backup_p(Header),%check if this is a checkpoint block.
    TH = headers:top(),
    B2 = Header#header.height > 
        (TH#header.height - 
             (7 * mft())),%2^7 > 100, less than 1% chance of no checkpoint in the range.
    X2 = if
             (B and B2) ->
                 Hash = block:hash(Header),
                 T = amoveo_sup:trees(),
                 CR = constants:custom_root(),
                 Tarball = CR ++ "backup.tar.gz",
                 Temp = CR ++ "backup_temp",
                 Temp2 = Temp ++ "2",
                 make_temp_dir(Temp),
                 make_temp_dir(Temp2),
                 ok = backup_trees(T, CR),%makes a copy of the tree files.
                 make_tarball(Tarball, Temp),%maybe we should do the packaging and zipping in erlang, so we don't need a sleep statement TODO
                 timer:sleep(200),
                 chunkify(Tarball, Temp2),%break up the gzipped tar file into 1 megabyte chunks, each in a different file.
                 remove_tarball(Tarball),%delete the gzipped file
                 move_chunks(Temp2, CR, Hash),
                 spawn(fun() ->
                               timer:sleep(1000),
                               clean(),
                               clean()
                       end),
                 os:cmd("rm -rf "++Temp),
                 CH2 = [Hash|
                        X#d.checkpoint_hashes],
                 X#d{checkpoint_hashes = CH2};
             true -> X
         end,
    {reply, ok, X2};
handle_call(recent, _From, X) -> 
    {reply, X#d.checkpoint_hashes, X};
handle_call(_, _From, X) -> {reply, X, X}.

make_temp_dir(Temp) ->
    os:cmd("mkdir " ++ Temp).%make temp file for backing up trees, also tells all the tree processes to make a backup on the hard drive of their current state.
remove_tree_copy(Temp) ->
    os:cmd("rm " ++ Temp ++ "/*").%delete the copy of the trees.
    
make_tarball(Tarball, Temp) ->
    S = "tar -czvf " ++ Tarball ++ " " ++ Temp,%make a gzipped tar of the copy of the tree files. 
    %io:fwrite(S),
    %io:fwrite("\n"),
    os:cmd(S).%make a gzipped tar of the copy of the tree files. 
remove_tarball(Tarball) ->
    spawn(fun() ->
                  timer:sleep(100),
                  os:cmd("rm " ++ Tarball)%delete the gzipped file
          end).
move_chunks(Temp, CR, Hash) ->
    Encoded = base58:binary_to_base58(Hash),
    os:cmd("mv " ++ Temp ++ " " ++ CR ++ "checkpoints/" ++ Encoded). %keep the chunks in a new folder in the checkpoints folder.
   
get_chunks(Hash, Peer, N) ->
    case talker:talk({checkpoint, Hash, N}, Peer) of
        {ok, D} -> 
            R = get_chunks(Hash, Peer, N+1),
            <<D/binary, R/binary>>;
        {error, "out of bounds"} ->
            <<>>;
        {error, E} ->
            io:fwrite("get_chunks unknown error\n"),
            io:fwrite(E)
    end.
test() -> 
    IP = {46,101,185,98},
    Port = 8080,
    sync(IP, Port).
sync(IP, Port) ->
    %set the config variable `reverse_syncing` to true.
    %let all the headers sync first before you run this.
    Peer = {IP, Port},
    CR = constants:custom_root(),
    {ok, CPL} = talker:talk({checkpoint}, Peer),
    CP1 = hd(lists:reverse(CPL)),%TODO, we should take the first checkpoint that is earlier than (top height) - (fork tolerance).

    Header = case headers:read(CP1) of
                 error ->
                     io:fwrite("we need to sync more headers first\n"),
                     1=2;
                 {ok, H} -> H
             end,
        
    Height = Header#header.height,
    TopHeader = headers:top(),
    {ok, Block} = talker:talk({block, Height-1}, Peer),
    {ok, NBlock} = talker:talk({block, Height}, Peer),
    TDB = Block#block.trees,
    TDBN = NBlock#block.trees,
    true = check_header_link(TopHeader, Header),
    Header = block:block_to_header(NBlock),
    {BDict, BNDict, BlockHash} = block:check0(Block),
    {NDict, NNewDict, CP1} = block:check0(NBlock),
    NBlock2 = NBlock#block{trees = {NDict, NNewDict, CP1}},
    Block2 = Block#block{trees = {BDict, BNDict, BlockHash}},
    Roots = NBlock#block.roots,
    TarballData = get_chunks(CP1, Peer, 0),
    Tarball = CR ++ "backup.tar.gz",
    file:write_file(Tarball, TarballData),
    Temp = CR ++ "backup_temp",
    S = "tar -C "++ CR ++" -xf " ++ Tarball,
    os:cmd(S),
    os:cmd("mv "++CR++"db/backup_temp/* " 
           ++ CR ++ "data/."),
    os:cmd("rm -rf "++ CR ++ "db"),
    os:cmd("rm " ++ CR ++ "backup.tar.gz"),
    TreeTypes = [accounts, channels, existence, oracles, governance, matched, unmatched],
    lists:map(fun(TN) -> trie:reload_ets(TN) end, TreeTypes),
    Roots = block:make_roots(TDB),
    lists:map(fun(Type) -> 
    %delete everything from the checkpoint besides the merkel trees of the one block we care about. Also verifies all the links in the merkel tree.
                      Pointer = trees:Type(TDB),
                      trie:clean_ets(Type, Pointer)
              end, TreeTypes),
    %try syncing the blocks between here and the top.
    block_hashes:add(CP1),
    {true, NBlock3} = block:check2(Block, NBlock2),
    %block_absorber:do_save(NBlock3, CP1),
    gen_server:cast(block_db, {write, Block, BlockHash}),
    gen_server:cast(block_db, {write, NBlock3, CP1}),
    block_db:set_ram_height(Height),
    headers:absorb_with_block([Header]),
    recent_blocks:add(CP1, 
                      Header#header.accumulative_difficulty, 
                      Height),
    tx_pool_feeder:dump(NBlock3),
    potential_block:dump(),
    sync:start(),
    {ok, ComPage0} = talker:talk({blocks, 50, Height}, Peer),
    Page0 = block_db:uncompress(ComPage0),
    Page = dict:filter(%remove data that is already in block_db.
             fun(_, Value) ->
                     Value#block.height < 
                         (Height - 1)
             end, Page0),
    CompressedPage = block_db:compress(Page),
    load_pages(CompressedPage, Block2, Roots, Peer).
load_pages(CompressedPage, BottomBlock, PrevRoots, Peer) ->
    %TODO start syncing blocks backward
    Page = block_db:uncompress(CompressedPage),
    {true, NewBottom} = verify_blocks(BottomBlock, Page, PrevRoots, length(dict:fetch_keys(Page))),
    %TODO
    %cut the DP into like 10 sub-lists, and make a process to verify each one. make sure there is 1 block of overlap, to know that the sub-lists are connected.
    %if a block has an unknown header, then drop this peer.
    %if any block is invalid, then lock the keys, and display a big error message.

    block_db:load_page(Page),
    StartHeight = NewBottom#block.height,
    if 
        StartHeight < 2 -> 
            io:fwrite("synced all blocks back to the genesis.\n"),
            ok;
        true -> 
            NextCompressed = talker:talk({blocks, 50, StartHeight}, Peer), %get next compressed page.
            load_pages(NextCompressed, NewBottom, BottomBlock#block.roots, Peer)
    end.
verify_blocks(B, _, _, 0) -> {true, B};
verify_blocks(B, P, PrevRoots, N) -> 
    io:fwrite("verify blocks "),
    io:fwrite(integer_to_list(N)),
    io:fwrite("\n"),
    {ok, NB} = dict:find(B#block.prev_hash, P),
    Proof = B#block.proofs,
    Roots = B#block.roots,
    {_NewDict4, NewDict3, _} = block:check3(NB, B),
    io:fwrite("verify blocks 2\n"),
    
    TreeTypes = [accounts, channels, existence, oracles, governance, matched, unmatched],
    RootsList0 = tl(tuple_to_list(Roots)),
    RootsList = 
        lists:map(
          fun(Tree) ->
                  io:fwrite("verify blocks 3\n"),
                  CFG = trie:cfg(Tree),
                  P2 = lists:filter(
                         fun(X) ->
                                 proofs:tree(X) == Tree
                         end, 
                         Proof),
                  UL = lists:map(
                         fun(X) ->
                                 io:fwrite("verify blocks 4\n"),
                                 K = proofs:key(X),
                                 %io:fwrite({K, dict:fetch_keys(NewDict3)}),
                                 V = Tree:dict_get(K, NewDict3),
                                 %V = trees:get(Tree, K, NewDict3, empty_trees),
                                 %io:fwrite({Tree, V}),
                                 %io:fwrite("\n"),
                                 if
                                     Tree == governance ->
                                         %io:fwrite(dict:fetch_keys(NewDict3)),
                                         %io:fwrite("\n"),
                                         ok;
                                     true -> ok
                                 end,
                                 SV = Tree:serialize(V),
                                 V2 = Tree:make_leaf(K, SV, CFG),
                                 {V2, proofs:path(X)}
                         end,
                         P2),
                  
                  io:fwrite("verify blocks 5\n"),
                  NewProofPaths = verify:update_proofs(UL, CFG),
                  io:fwrite("verify blocks 6\n"),
                  case NewProofPaths of
                      [] -> unchanged;
                      [H|_] -> 
                          S = hd(lists:reverse(H)),
                          stem:hash(S, CFG)
                  end
          end, TreeTypes),
    io:fwrite("verify blocks 7\n"),
    
    Roots2 = [roots2|RootsList],
    true = check_roots_match(Roots2, 
                             tuple_to_list(PrevRoots)),
    {NDict, NNewDict, Hash} = block:check0(NB),
    NB2 = NB#block{trees = {NDict, NNewDict, Hash}},
    verify_blocks(NB2, P, Roots, N-1).
check_header_link(Top, New) ->
    TH = Top#header.height,
    NH = New#header.height,
    if
        (NH > TH) -> false;
        (NH == TH) -> 
            block:hash(Top) == block:hash(New);
        true -> 
            {ok, Prev} = headers:read(Top#header.prev_hash),
            check_header_link(Prev, New)
    end.
       
check_roots_match([], []) -> 
    true;
check_roots_match([A|T1], [A|T2]) -> 
    check_roots_match(T1, T2);
check_roots_match([unchanged|T1], [_|T2]) -> 
    check_roots_match(T1, T2);
check_roots_match(_, _) -> 
    false.

clean_helper([]) -> [];
clean_helper([X]) -> [X];
clean_helper(L) -> 
    L2 = lists:reverse(L),
    [CP1|[CP2|T]] = L2,
    TopHeader = headers:top_with_block(),
    THHeight = TopHeader#header.height,
    Cutoff = THHeight - mft(),
    {ok, H2} = headers:read(CP2),
    {ok, H1} = headers:read(CP1),
    H2Height = H2#header.height,
    H1Height = H1#header.height,
    CPL = filtered([CP2]),
    CR = constants:custom_root(),
    L3 = if
             ((H2Height < Cutoff) 
              and (H1Height < Cutoff)
              and (CPL == [])) ->
                 %remove uncle checkpoint.
                 Encoded2 = base58:binary_to_base58(CP2),
                 spawn(fun() ->
                               os:cmd("rm -r " ++ CR ++ "checkpoints/"++Encoded2)
                       end),
                 [CP1|T];
             ((H2Height < Cutoff) 
              and (H1Height < Cutoff)) ->
                 Encoded1 = base58:binary_to_base58(CP1),
                 CR = constants:custom_root(),
                 spawn(fun() ->
                               os:cmd("rm -r " ++ CR ++ "checkpoints/"++Encoded1)
                       end),
                 [CP2|T];
             true ->
                 [CP1|[CP2|T]]
         end,
    lists:reverse(L3).
chunkify(File, Folder) ->
    case file:read_file(File) of
        {ok, D} ->
            chunkify2(D, Folder, 0);
        {error, _} ->
            1=2,
            ok
    end.
chunkify2(<<>>, _, _) -> ok;
chunkify2(<<S:8388608, R/binary>>, F, N) -> 
    %8388608 is 1 megabyte.
    file:write_file(F++chunk_name(N), <<S:8388608>>),
    chunkify2(R, F, N+1);
chunkify2(R, F, N) -> 
    file:write_file(F ++ chunk_name(N), R),
    ok.
chunk_name(N) ->
    "/" ++ integer_to_list(N) ++ 
        ".checkpoint.chunk".
backup_p(Header) ->
    <<H:256>> = block:hash(Header),
    B = (H rem mft()),
    B == 0.
cp(CR, H, S) ->
    Name = atom_to_list(H)++S,
    os:cmd("cp " ++ CR ++ "data/" ++ Name ++ " " 
           ++ CR ++ "backup_temp/" ++ Name).
backup_trees([], _) -> 
    ok;
backup_trees([H|T], CR) -> 
    %gen_server:call({global, ids:main_id(H)}, fail),
    trie:quick_save(H),
    cp(CR, H, ".db"),
    cp(CR, H, "_rest.db"),
    backup_trees(T, CR).


make() ->
    gen_server:call(?MODULE, make).
    
clean() ->
    gen_server:cast(?MODULE, clean).

filtered(L) ->
    lists:filter(fun(Hash) ->
                         %only share hashes for blocks that are on the longest chain of headers.
                         {ok, H} = headers:read(Hash),
                         B = block:get_by_height(H#header.height),
                         Hash2 = block:hash(B),
                         Hash2 == Hash
                 end, L).
    
    
recent() ->
    L = gen_server:call(?MODULE, recent),
    filtered(L).

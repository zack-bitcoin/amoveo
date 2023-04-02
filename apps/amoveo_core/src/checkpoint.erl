-module(checkpoint).
-behaviour(gen_server).
-export([backup_p/1,
         make/0, %check if the top header with a block needs a checkpoint, and if so, makes it.
         recent/0, %returns a list of recent block hashes where the block is a checkpoint.
         clean/0, %deletes old unneeded checkpoints.
         chunk_name/1,
         sync/2,
         sync/0,
         reverse_sync/0,
         reverse_sync/2, 
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
sync() -> 
    block_db:set_ram_height(0),
    IP = {46,101,185,98},
    %IP = {159,89,87,58},
    Port = 8080,
    spawn(fun() ->
                  sync(IP, Port)
          end).
sync({IP, Port}) ->
    sync(IP, Port).
sync(IP, Port) ->
    %set the config variable `reverse_syncing` to true.
    %let all the headers sync first before you run this.
    Peer = {IP, Port},
    CR = constants:custom_root(),
    io:fwrite("downloading checkpoint\n"),
    {ok, CPL} = talker:talk({checkpoint}, Peer),
    io:fwrite("unpacking checkpoint\n"),
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
    {BDict, BNDict, BProofTree, BlockHash} = block:check0(Block),
    {NDict, NNewDict, NProofTree, CP1} = block:check0(NBlock),
    NBlock2 = NBlock#block{trees = {NDict, NNewDict, NProofTree, CP1}},
    Block2 = Block#block{trees = {BDict, BNDict, BProofTree, BlockHash}},
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
    TreeTypes = tree_types(element(1, TDB)),
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
    reverse_sync2(Height, Peer, Block2, Roots).

reverse_sync() ->
    %find a peer that has a checkpoint.
    spawn(fun() ->
                  Ps = peers:all(),
                  Ps2 = sync:shuffle(Ps),
                  P = hd(Ps2),
                  case talker:talk(
                         {checkpoint}, P) of
                      {ok, CPL} -> reverse_sync(P);
                      X -> io:fwrite(X)
                  end
          end).

reverse_sync(Peer) ->
    Height = block:bottom() + 1,
    reverse_sync(Height, Peer).

reverse_sync(Height, Peer) ->
    io:fwrite("reverse_sync\n"),
    {ok, Block} = talker:talk({block, Height-1}, Peer),
    {ok, NBlock} = talker:talk({block, Height}, Peer),
    Roots = NBlock#block.roots,

    {BDict, BNDict, BProofTree, BlockHash} = block:check0(Block),
    Block2 = Block#block{trees = {BDict, BNDict, BProofTree, BlockHash}},
    %TDB = Block#block.trees,
    %Roots = block:make_roots(TDB),
    reverse_sync2(Height, Peer, Block2, Roots).


reverse_sync2(Height, Peer, Block2, Roots) ->
    io:fwrite("reverse_sync2\n"),
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
    Page = block_db:uncompress(CompressedPage),
    {true, NewBottom, NextRoots} = verify_blocks(BottomBlock, Page, PrevRoots, length(dict:fetch_keys(Page))),
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
            {ok, NextCompressed} = talker:talk({blocks, 50, StartHeight-2}, Peer), %get next compressed page.
            %load_pages(NextCompressed, NewBottom, BottomBlock#block.roots, Peer)
            load_pages(NextCompressed, NewBottom, NextRoots, Peer)
    end.
tree_types(trees5) -> [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets, receipts, stablecoins];
tree_types(trees4) -> [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets];
tree_types(trees3) -> [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades];
tree_types(trees2) -> [accounts, channels, existence, oracles, governance, matched, unmatched];
tree_types(trees) -> [accounts, channels, existence, oracles, governance].
verify_blocks(B, _, Roots, 0) -> {true, B, Roots};
verify_blocks(B, P, PrevRoots, N) -> 
    Height = B#block.height,
    if
        ((Height rem 100) == 0) ->
            io:fwrite("absorb in reverse " ++
                          integer_to_list(B#block.height) ++
                          "\n");
        true -> ok
    end,
    {ok, NB} = dict:find(B#block.prev_hash, P),

    Proof = B#block.proofs,
    %io:fwrite("verify blocks "),
    %io:fwrite(NB#block.trees),
    %io:fwrite("\n"),
    TreeTypes = tree_types(element(1, NB#block.trees)),
    {CRM, Leaves} = 
        if
            (B#block.height < 38700) -> 
                {true, []};
            (B#block.height < 109000) -> 
                {_, _NewDict3, _, _} = block:check3(NB, B),
                {true, []};
            true ->
                {_, NewDict3, _, _} = block:check3(NB, B),
                
                {RootsList, Leaves0} = calc_roots2(TreeTypes, Proof, dict:fetch_keys(NewDict3), NewDict3, [], []),
                %Roots2 = [roots2|RootsList],
                Bool = 
                    check_roots_match(
                      RootsList, 
                      tl(tuple_to_list(PrevRoots))),
                if
                    not(Bool) -> 
                        io:fwrite({{height, Height}, {lengths, length(RootsList), length(tl(tuple_to_list(PrevRoots))), length(TreeTypes)}, {roots_list, RootsList}, {prev_roots, PrevRoots}, {tree_types, TreeTypes}});
                    true -> ok
                end,
                {Bool, Leaves0}
        end,
    case CRM of
        true -> ok;
        false ->
            %io:fwrite("dict3 keys: "),
            %io:fwrite("\n"),
            %io:fwrite(packer:pack(dict:fetch_keys(NewDict3))),%[{unmatched, {key, Pub, ID}},{governance, 1}|...]
            %io:fwrite("\n"),
            %io:fwrite("all proofs: "),
            %io:fwrite(packer:pack(Proof)),
            %io:fwrite("\n"),
            %io:fwrite(packer:pack({Roots2, tuple_to_list(PrevRoots)})),
            %io:fwrite("\n"),
            CFG = trie:cfg(unmatched),
            P2 = lists:filter(
                   fun(X) ->
                           proofs:tree(X) == unmatched
                   end, Proof),
            P3 = lists:map(
                   fun(X) ->
                           proofs:value(X)
                   end, P2),
            Leaves2 = leaf_vals(TreeTypes, Leaves),
            %io:fwrite(packer:pack(Leaves)),
            io:fwrite(packer:pack(Leaves2)),
            io:fwrite("\n"),
            1=2
    end,
    {NDict, NNewDict, NProofTree, Hash} = block:check0(NB),
    NB2 = NB#block{trees = {NDict, NNewDict, NProofTree, Hash}},
    verify_blocks(NB2, P, B#block.roots, N-1).
leaf_vals([], []) -> [];
leaf_vals([Tree|T], [Leafs|L]) ->
    V = lists:map(
          fun(Leaf) ->
                  LV = leaf:value(Leaf),
                  K = leaf:key(Leaf),
                  case LV of
                      empty -> empty;
                      _ -> Tree:deserialize(LV)
                  end
          end, Leafs),
    [{Tree, V}|leaf_vals(T, L)].
calc_roots2([], _, _, _, R, L) ->
    {lists:reverse(R), 
     lists:reverse(L)};
calc_roots2([Tree|TT], Proof, Keys, NewDict3, RL, LL) ->
    CFG = trie:cfg(Tree),
    K2 = lists:filter(
           fun({Tree2, _K}) ->
                   Tree2 == Tree;
              (X) -> io:fwrite({X}),
                     1=2
           end,
               
           Keys),%[{unmatched, {key, Pub, OID}}|...]
    P2 = lists:filter(
           fun(X) ->
                   proofs:tree(X) == Tree
           end, 
           Proof),
    {R, UL2} = case K2 of
            [] -> {0, []};
            _ ->
                StemHashes = hd(lists:reverse(proofs:path(hd(P2)))),
                RS = stem:make(StemHashes, Tree),
                RootHash = stem:hash(RS, CFG),
                {M, Root} = mtree:new_restoration(RS, CFG),
                {M2, Root2} = restore_all(P2, RootHash, Root, Tree, CFG, M),
                UL = leaf_maker2(K2, NewDict3, CFG),
                {Root3, M3} = mtree:store_batch(UL, Root2, M2),
                {mtree:root_hash(Root3, M3),
                 UL}
        end,
    calc_roots2(TT, Proof, Keys, NewDict3, [R|RL], [UL2|LL]).

    
calc_roots([], _, _, R, L) -> 
    {lists:reverse(R), 
     lists:reverse(L)};
calc_roots([Tree|TT], Proof, NewDict3, RL, LL) -> 
    CFG = trie:cfg(Tree),
    P2 = lists:filter(
           fun(X) ->
                   proofs:tree(X) == Tree
           end, 
           Proof),
    {R, UL2} = case P2 of
            [] -> {0, []};
            _ ->
                StemHashes = hd(lists:reverse(proofs:path(hd(P2)))),
                RS = stem:make(StemHashes, Tree),
                RootHash = stem:hash(RS, CFG),
                {M, Root} = mtree:new_restoration(RS, CFG),
                {M2, Root2} = restore_all(P2, RootHash, Root, Tree, CFG, M),
                UL = leaf_maker(P2, NewDict3, Tree, CFG),
                {Root3, M3} = mtree:store_batch(UL, Root2, M2),
                {mtree:root_hash(Root3, M3),
                 UL}
        end,
    calc_roots(TT, Proof, NewDict3, [R|RL], [UL2|LL]).
        
leaf_maker2([], _, _) -> [];
leaf_maker2([{Tree, K}|P2], NewDict3, CFG) ->
    %the values of the leaves after the block is processed.
    %io:fwrite(packer:pack({Tree, K})),
    %io:fwrite("\n"),
    PS = constants:pubkey_size() * 8,
    SV = case {K, Tree} of
             {{key, <<1:PS>>, OID}, unmatched} ->%its a unmatched header
                 {P, Many} = unmatched:dict_head_get(NewDict3, OID),
                 unmatched:serialize_head(P, Many);
             _ ->
                 V = Tree:dict_get(K, NewDict3),
                 case V of
                     empty -> empty;
                     _ -> Tree:serialize(V)
                 end
         end,
    L = Tree:make_leaf(K, SV, CFG),
    [L|leaf_maker2(P2, NewDict3, CFG)].
    
leaf_maker([], _, _, _) -> [];
leaf_maker([X|P2], NewDict3, Tree, CFG) ->
    %the values of the leaves after the block is processed.
    K = proofs:key(X),
    %io:fwrite(packer:pack(K)),
    %io:fwrite("\n"),
    PS = constants:pubkey_size() * 8,
    SV = case {K, Tree} of
             {{key, <<1:PS>>, OID}, unmatched} ->%its a unmatched header
                 {P, Many} = unmatched:dict_head_get(NewDict3, OID),
                 unmatched:serialize_head(P, Many);
             _ ->
                 V = Tree:dict_get(K, NewDict3),
                 case V of
                     empty -> empty;
                     _ -> Tree:serialize(V)
                 end
         end,
    L = Tree:make_leaf(K, SV, CFG),
    [L|leaf_maker(P2, NewDict3, Tree, CFG)].
    
restore_all([], _, Root, _, _, M) -> {M, Root};
restore_all([P|T], Hash, Root, Tree, CFG, M) -> 
    %the merkle tree before the block is processed
    V0 = proofs:value(P),
    V = case V0 of
            0 -> empty;
            V1 -> V1
        end,
    Path = proofs:path(P),
    Key = proofs:key(P),
    Leaf = Tree:make_leaf(Key, V, CFG),
%new(Key, Value, Meta, CFG) ->
    true = verify:proof(Hash, Leaf, Path, CFG),
    %io:fwrite({Leaf, Hash, Path, Root, M}),
    %{Leaf, Hash, [{16 hashes}|...]}, 1, M}
    {_, Root2, _, M2} = 
        %path should be "proof"
        mtree:restore(Leaf, Hash, Path, Root, M),
    restore_all(T, Hash, Root2, Tree, CFG, M2).
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
check_roots_match([0|T1], [_|T2]) -> 
    check_roots_match(T1, T2);
check_roots_match(X1, X2) -> 
    %io:fwrite("roots don't match \n"),
    %io:fwrite({X1, X2}),
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

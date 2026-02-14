-module(checkpoint).
-behaviour(gen_server).
-export([backup_p/1,
         make/0,
         make/1, %check if the top header with a block needs a checkpoint, and if so, makes it.
         recent/0, %returns a list of recent block hashes where the block is a checkpoint.
         clean/0, %deletes old unneeded checkpoints.
         chunk_name/1,
         sync/3,
         sync/2,
         sync/0,
         sync_hardcoded/0,
         reverse_sync/0,
         reverse_sync/1,
         reverse_sync/2, 
         full_tree_merkle/0, full_tree_merkle/1,
         sync/1,
         scan_blocks/0,
         mem_check/0,
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
handle_call({make, Force}, _, X) -> 
    Header = headers:top_with_block(),
    B = Force or backup_p(Header),%check if this is a checkpoint block.
    TH = headers:top(),
    Height = Header#header.height,

    %only create checkpoints for recent blocks.
    B2 = Height > 
        (TH#header.height - 
             (7 * mft())),
    X2 = if
             (B and B2) ->
                 Hash = block:hash(Header),
                 Block = block:get_by_hash(Hash),
                 Pointer = Block#block.trees,
                 trees2:scan_verkle(Pointer, tree:cfg(amoveo)),
                 spawn(fun() ->
                 F52 = forks:get(52),
                 T = amoveo_sup:trees(),
                 CR = constants:custom_root(),
                 Tarball = CR ++ "backup.tar.gz",
                 Temp = CR ++ "backup_temp",
                 Temp2 = Temp ++ "2",
                 make_temp_dir(Temp),
                 make_temp_dir(Temp2),
                 if
                     Height < F52 ->
                         backup_trees(T, CR),
                         ok;%makes a copy of the tree files.
                     true ->
                         tree:quick_save(amoveo),
                         VerkleTrees = ["accounts", "contracts", "markets", "matched", "oracles", "receipts", "sub_accs", "trades", "unmatched", "jobs", "futarchy", "futarchy_unmatched", "futarchy_matched"],
                         io:fwrite("quicksaving dumps\n"),
                         lists:map(fun(S) ->
                                           io:fwrite(S),
                                           io:fwrite("\n"),
                                           A = list_to_atom(S ++ "_dump"),
                                           dump:quick_save(A)
                                   end, VerkleTrees),
                         io:fwrite("quicksaved dumps\n"),
                         %Mode = verkle_trees_sup:mode(),
                         %case Mode of
                         %    ram ->
                         %        tree:quick_save(amoveo);
                         %    hd ->
                         %        ok
                         %end,
                         os:cmd("cp " ++ CR ++ "data/amoveo_v_leaf.db " ++ Temp ++ "/amoveo_v_leaf.db"),
                         os:cmd("cp " ++ CR ++ "data/amoveo_v_leaf_rest.db " ++ Temp ++ "/amoveo_v_leaf_rest.db"),
                         os:cmd("cp " ++ CR ++ "data/amoveo_v_leaf_bits.db " ++ Temp ++ "/amoveo_v_leaf_bits.db"),
                         os:cmd("cp " ++ CR ++ "data/amoveo_v_stem.db " ++ Temp ++ "/amoveo_v_stem.db"),
                         os:cmd("cp " ++ CR ++ "data/amoveo_v_stem_rest.db " ++ Temp ++ "/amoveo_v_stem_rest.db"),
                         os:cmd("cp " ++ CR ++ "data/amoveo_v_stem_bits.db " ++ Temp ++ "/amoveo_v_stem_bits.db"),
                         lists:map(fun(X) ->
                                           os:cmd("cp " ++ CR ++ "data/" ++ X ++ ".db " ++ Temp ++ "/" ++ X ++ ".db"),
                                           os:cmd("cp " ++ CR ++ "data/" ++ X ++ "_rest.db " ++ Temp ++ "/" ++ X ++ "_rest.db"),
                                           os:cmd("cp " ++ CR ++ "data/" ++ X ++ "_dump.db " ++ Temp ++ "/" ++ X ++ "_dump.db")
                                   end, VerkleTrees)
                 end,
                 make_tarball(Tarball, Temp),%maybe we should do the packaging and zipping in erlang, so we don't need a sleep statement TODO
                 timer:sleep(200),%maybe this isn't necessary? seems like os:cmd waits for the command to finish before returning.
                 chunkify(Tarball, Temp2),%break up the gzipped tar file into 1 megabyte chunks, each in a different file.
                 io:fwrite("deleting tarball\n"),
                 remove_tarball(Tarball),%delete the gzipped file
                 io:fwrite("moving chunks\n"),
                 move_chunks(Temp2, CR, Hash),
%                 spawn(fun() ->
                               timer:sleep(1000),
                               %clean(),
                               clean()
                       end),
                 %os:cmd("rm -rf "++Temp),
                 CH2 = [Hash|
                        X#d.checkpoint_hashes],
                 io:fwrite("updating checkpoint hashes\n"),
                 timer:sleep(1000),
                 X#d{checkpoint_hashes = CH2};
             true -> X
         end,
    {reply, ok, X2};
handle_call(recent, _From, X) -> 
    {reply, X#d.checkpoint_hashes, X};
handle_call(_, _From, X) -> {reply, X, X}.

make_temp_dir(Temp) ->
    os:cmd("mkdir " ++ Temp).%make temp file for backing up trees, also tells all the tree processes to make a backup on the hard drive of their current state.
    
make_tarball(Tarball, Temp) ->
    io:fwrite("making tarball\n"),
    S = "tar -czvf " ++ Tarball ++ " " ++ Temp,%make a gzipped tar of the copy of the tree files. 
    io:fwrite(S),
    io:fwrite("\n"),
    os:cmd(S).%make a gzipped tar of the copy of the tree files. 
remove_tarball(Tarball) ->
    spawn(fun() ->
                  os:cmd("rm " ++ Tarball)%delete the gzipped file
          end).
move_chunks(Temp, CR, Hash) ->
    Encoded = base58:binary_to_base58(Hash),
    os:cmd("mv " ++ Temp ++ " " ++ CR ++ "checkpoints/" ++ Encoded). %keep the chunks in a new folder in the checkpoints folder.
   
get_chunks_old(Hash, Peer, N) ->
    case talker:talk({checkpoint, Hash, N}, Peer) of
        {ok, D} -> 
            io:fwrite("got chunk\n"),
            io:fwrite(integer_to_list(N)),
            io:fwrite("\n"),
            io:fwrite("memory usage: " ++ integer_to_list(erlang:memory(total))),
            R = get_chunks(Hash, Peer, N+1),
            <<D/binary, R/binary>>;
        {error, "out of bounds"} ->
            <<>>;
        {error, E} ->
            io:fwrite("get_chunks unknown error\n"),
            io:fwrite(E)
    end.
get_chunks(Hash, Peer, N) ->
    get_chunks2(Hash, Peer, N, []).
get_chunks2(Hash, Peer, N, Result) ->
    case talker:talk({checkpoint, Hash, N}, Peer) of
        {ok, D} -> 
            io:fwrite("got chunk"),
            io:fwrite(integer_to_list(N)),
            io:fwrite("\n"),
            get_chunks2(Hash, Peer, N+1, [D|Result]);
        {error, "out of bounds"} ->
            io:fwrite("last chunk\n"),
            R2 = lists:reverse(Result),
            list_to_binary(R2);
        {error, E} ->
            io:fwrite("get_chunks2 unknown error\n"),
            io:fwrite(E)
    end.
    


sync_hardcoded() -> 
    %IP = {159,223,85,216},%the pool
    IP = {46,101,81,5},%explorer
    %IP = {159,65,126,146},%germany
    %IP = {64,227,100,178}, % san francisco
    Port = 8080,
    spawn(fun() ->
                  sync(IP, Port)
          end).
sync() ->
    spawn(fun() ->
                  Ps = peers:all(),
                  Ps2 = sync:shuffle(Ps),
                  P = hd(Ps2),
                  checksync(P)
          end).
checksync(P = {IP, Port}) ->
    %io:fwrite("checksync\n"),
    sync_kill:start(),
    Y = talker:talk(
          {checkpoint}, P),
    timer:sleep(100),
    case Y of
        {ok, []} -> 
            io:fwrite("This peer doesn't have any checkpoints. Attempting to find a different peer\n"),
            sync();
        {ok, CPL} -> sync(P, CPL);
        X -> io:fwrite(X),
             sync()
    end.
    
sync({IP, Port}, CPL) ->
    sync(IP, Port, CPL);
sync(IP, Port) ->
    checksync({IP, Port}).
sync({IP, Port}) ->
    sync(IP, Port).

sync(IP, Port, CPL0) ->
    %CPL = lists:reverse(CPL0),
    CPL = CPL0,
    %set the config variable `reverse_syncing` to true.
    %let all the headers sync first before you run this.
    Peer = {IP, Port},
    {ok, ForkTolerance} = 
        application:get_env(
          amoveo_core, fork_tolerance),
    CR = constants:custom_root(),
    io:fwrite("searching for a checkpoint, trying: "),
    %64 227 21 70
    case IP of
        {IPA, IPB, IPC, IPD} ->
            String01 = 
            integer_to_list(IPA) ++
            "."++
            integer_to_list(IPB) ++
            "."++
            integer_to_list(IPC) ++
            "."++
            integer_to_list(IPD),
            io:fwrite(String01),
            ok;
        _ -> io:fwrite(IP)
    end,
    io:fwrite("\n"),

    HCPL0 = lists:map(
             fun(C) ->
                     case headers:read(C) of
                         {ok, H} ->
                             H2 = H#header.height,
                             {H2, C};
                         error ->
                             {}
                     end
             end, CPL),
    TopHeight = api:height(),
    HCPL = lists:filter(fun(X) ->
                                case X of
                                    {H, _} -> %H < (TopHeight - ForkTolerance);
                                        true;
                                    _ -> false
                                end
                        end, HCPL0),
    case HCPL of
        [] -> 
            io:fwrite("this peer doesn't have a checkpoint at an early enough height to conform to the security configuration of this node. Attempting to find a different peer...\n"),
            sync();
        _ ->
            %{_, CP1} = hd(lists:reverse(HCPL)),
            {_, CP1} = hd(HCPL),

            Header = case headers:read(CP1) of
                         error ->
                             io:fwrite("we need to sync more headers first\n"),
                             1=2;
                         {ok, H} -> H
                     end,
        
            Height = Header#header.height,
            PrintString =
                "Checkpoint height is " ++
                integer_to_list(Height) ++
                "\n" ++
                "hash is " ++
                base58:binary_to_base58(CP1) ++
                ", now loading checkpoint\n",
            io:fwrite(PrintString),
            TopHeader = headers:top(),
            io:fwrite("checkpoint sync get block 1\n"),
            {ok, Block} = talker:talk({block, Height-1}, Peer),
            io:fwrite("checkpoint sync get block 2\n"),
            {ok, NBlock} = talker:talk({block, Height}, Peer),
            io:fwrite("checkpoint sync get block 3\n"),
            TDB = Block#block.trees,
            TDBN = NBlock#block.trees,
            true = check_header_link(TopHeader, Header),
            Header2 = block:block_to_header(NBlock),
            if
                (not(Header == Header2)) ->
                    io:fwrite({Header, Header2}),
                    ok;
                true -> ok
            end,
            {BDict, BNDict, BProofTree, BlockHash} = block:check0(Block),
            {NDict, NNewDict, NProofTree, CP1} = block:check0(NBlock),
            NBlock2 = NBlock#block{trees = {NDict, NNewDict, NProofTree, CP1}},
            Block2 = Block#block{trees = {BDict, BNDict, BProofTree, BlockHash}},
            Roots = NBlock#block.roots,
            io:fwrite("Found a candidate checkpoint. downloading... \n"),
            io:fwrite("memory usage: " ++ integer_to_list(erlang:memory(total))),
            TarballData = get_chunks(CP1, Peer, 0),
            io:fwrite("Found a candidate checkpoint, got chunks. \n"),
            Tarball = CR ++ "backup.tar.gz",
            file:write_file(Tarball, TarballData),
            Temp = CR ++ "backup_temp",
            io:fwrite("unzipping the checkpoint\n"),
            S = "tar -xf " ++ Tarball ++ " -C " ++ Temp,
            os:cmd("mkdir " ++ Temp),
            os:cmd(S),
            if
                is_integer(TDB) ->
                    os:cmd("mv " ++ Temp ++ "/db/backup_temp/*.db " ++ CR ++ "data/."),
                    os:cmd("rm -rf "++ Temp), %%
                    os:cmd("rm "++ Tarball), %%
                    
                    io:fwrite("verkle checkpoint\n"),
                    ID = amoveo,
                    Pointer = TDBN,
                    CFG = tree:cfg(ID),
                    timer:sleep(1000),
                    tree:reload_ets(ID),
                    timer:sleep(1000),
                    Stem0 = stem_verkle:get(Pointer, CFG),
                    io:fwrite("checkpoint lookup root integrity " ++ integer_to_list(Pointer) ++ "\n"),
                    case stem_verkle:check_root_integrity(Stem0) of
                        success -> ok;
                        _ -> 
                            io:fwrite("invalid root stem\n"),
                            io:fwrite(Stem0)
                    end,
                    Types = element(3, Stem0),
                    NRoots = tree:root_hash(ID, Pointer),
                    NRoots2 = NBlock2#block.trees_hash,
                    if
                        (NRoots2 == NRoots) -> ok;
                        true -> io:fwrite("nroots did not match!\n"),
                                io:fwrite({NRoots, NRoots2, NBlock2#block.height})
                    end;
                true ->
                    io:fwrite("loading a merkle checkpoint.\n"),
                    %io:fwrite(Tarball),
                    %io:fwrite("\n,
                    %io:fwrite(Temp),
                    %io:fwrite("\n"),
                    %os:cmd("mv "++ Temp ++ "/* " ++ CR ++ "data/."),
                    %io:fwrite("test -d " ++Temp ++ "/backup_temp && echo \"yes\""),
                    case os:cmd("test -d " ++Temp ++ "/backup_temp && echo \"yes\"") of
                        "yes\n" ->
                            io:fwrite("new zip format\n"),
                            os:cmd("mv "++ Temp ++ "/backup_temp/* " ++ CR ++ "data/."),
                            io:fwrite("mv "++ Temp ++ "/backup_temp/* " ++ CR ++ "data/.\n");
                        X ->
                            io:fwrite("old zip format\n"),
                            os:cmd("cp "++ Temp ++ "/db/backup_temp/* " ++ CR ++ "data/."),
                            io:fwrite("cp "++ Temp ++ "/db/backup_temp/* " ++ CR ++ "data/.")
                    end,
                            
                    os:cmd("rm -rf "++ Temp),
                    os:cmd("rm "++ Tarball),



                    %TreeTypes = tree_types(element(1, TDB)),
                    TreeTypes = tree_types_by_height(Height),

    %TDB is trees from the old block.
                    %timer:sleep(500),
                    %io:fwrite("about to reload ets\n"),

    %todo. what if a page is empty? we need to load an empty table with the correct configuration.
    %the configuration data is in a bunch of tree_child/6 in amoveo_sup. 

                    lists:map(fun(TN) -> trie:reload_ets(TN) end, TreeTypes),%grabs the copy of the table from the hard drive, and loads it into ram.
                    %timer:sleep(2000),
                    %io:fwrite("reloaded ets\n"),
                    MRoots = block:make_roots(TDB),%this works because when we downloaded the checkpoint from them, it is the same data being stored at the same pointer locations.
                    io:fwrite("made roots\n"),
                    lists:map(fun(Type) -> 
    %delete everything from the checkpoint besides the merkel trees of the one block we care about. Also verifies all the links in the merkel tree.
                                      Pointer = trees:Type(TDB),
                              %trie:clean_ets(Type, Pointer)
                                      ok
                              end, TreeTypes),
                    true = full_tree_merkle(TDB, TreeTypes),
              Roots = MRoots
            end,
            block_hashes:add(CP1),
            {true, NBlock3} = block:check2(Block, NBlock2),
            gen_server:cast(block_db, {write, Block, BlockHash}),
    gen_server:cast(block_db, {write, NBlock, CP1}),
            %headers:absorb_with_block([Header]),
            gen_server:call(headers, {add_with_block, block:hash(Header), Header}, infinity),
    tx_pool_feeder:dump(NBlock),
    potential_block:dump(),
            timer:sleep(1000),
            if 
                is_integer(TDB) ->
                    Pointerb = NBlock#block.trees,
                    
                    dump:reload(accounts_dump),
                    dump:reload(contracts_dump),
                    dump:reload(markets_dump),
                    dump:reload(matched_dump),
                    dump:reload(oracles_dump),
                    dump:reload(receipts_dump),
                    dump:reload(sub_accs_dump),
                    dump:reload(trades_dump),
                    dump:reload(unmatched_dump),
                    dump:reload(jobs_dump),
                    dump:reload(futarchy_dump),
                    dump:reload(futarchy_unmatched_dump),
                    dump:reload(futarchy_matched_dump),

                    trees2:scan_verkle(Pointerb, tree:cfg(amoveo)),
                    io:fwrite("scanned 2\n"),
                    Stem1 = stem_verkle:get(Pointerb, tree:cfg(amoveo)),
                    NewPointer = trees2:one_root_clean(Pointerb, tree:cfg(amoveo)),
                    io:fwrite("cleaned\n"),
                    trees2:scan_verkle(NewPointer, tree:cfg(amoveo)),
                    io:fwrite("scan cleaned\n"),
                    Stem2 = stem_verkle:get(NewPointer, tree:cfg(amoveo)),
                    StemHashb = stem_verkle:hash(Stem1),
                    StemHashb = stem_verkle:hash(Stem2),
                    BlockB = NBlock#block{trees = NewPointer},
                    %gen_server:cast(block_db, {write, BlockB, CP1}),
                    block_db3:write(BlockB, CP1),
                    block_db3:set_top(CP1),
                    io:fwrite("successfully updated the block\n"),
                    ok;
                true -> ok
            end,

    %io:fwrite("checkpoint starting reverse sync\n"),
    %timer:sleep(100),
    %reverse_sync2(Height, Peer, Block2, Roots),
    %reverse_sync(Peer),
            timer:sleep(1000),
            %make(true),
            %timer:sleep(1000),
            io:fwrite("checkpoint starting sync\n"),
            sync:start([{IP, Port}]),
            ok
    end.
    %ok.

full_tree_merkle() ->
    full_tree_merkle(block:top()).

full_tree_merkle(Block = #block{height = Height, trees = Trees}) ->
    TreeTypes = tree_types_by_height(Height),
    full_tree_merkle(Trees, TreeTypes).
%full_tree_merkle(Block = #block{trees = Trees}) ->
%    full_tree_merkle(Trees);
%full_tree_merkle(Trees) ->
%    TreeTypes = tree_types(element(1, Trees)),
%    full_tree_merkle(Trees, TreeTypes).

full_tree_merkle(Trees, TTs) ->
    %we don't need a verkle version, because tree:clean_ets does that.
    R = lists:map(fun(Type) ->
                          Pointer = trees:Type(Trees),
                          trie:integrity_check(Pointer, Type)
                  end, TTs),
    %io:fwrite(R),
    R2 = lists:foldl(fun(X, A) ->
                             X and A
                     end, true, R),
    if
        R2 -> true;
        true -> io:fwrite({R, TTs}),
                false
    end.
            


all_zero([]) -> true;
all_zero([0|T]) -> 
    all_zero(T);
all_zero(_) -> false.

reverse_sync() ->
    io:fwrite("reverse sync/0\n"),
    sync_kill:start(),
    %find a peer that has a checkpoint.
    spawn(fun() ->
                  Ps = peers:all(),
                  Ps2 = sync:remove_self(
                          sync:shuffle(Ps)),
                  P = hd(Ps2),
                  case talker:talk(
                         {checkpoint}, P) of
                      {ok, CPL} -> reverse_sync(P);
                      X -> 
                          io:fwrite("reverse_sync failure 0\n"),
                          io:fwrite(X)
                  end
          end).

reverse_sync(Peer) ->
    io:fwrite("reverse sync/1\n"),
    spawn(fun() ->
                  Height = block:bottom(),
                  reverse_sync(Height, Peer)
          end).

reverse_sync(Height, Peer) ->
    io:fwrite("reverse sync/2\n"),
    case Peer of
        {{P1, P2, P3, P4}, _} ->
            io:fwrite("peer: "),
            io:fwrite(integer_to_list(P1)),
            io:fwrite("."),
            io:fwrite(integer_to_list(P2)),
            io:fwrite("."),
            io:fwrite(integer_to_list(P3)),
            io:fwrite("."),
            io:fwrite(integer_to_list(P4)),
            io:fwrite("\n");
        _ -> io:fwrite({Peer})
    end,
    sync_kill:start(),
    io:fwrite("reverse sync/2 got block 0\n"),
    io:fwrite(integer_to_list(Height)),
    io:fwrite("\n"),
    %{ok, Block} = talker:talk({block, Height-1}, Peer),%same as bottom.
    {ok, Block} = talker:talk({block, Height}, Peer),%same as bottom.
    io:fwrite("reverse sync/2 got block 1\n"),
    %{ok, NBlock} = talker:talk({block, Height}, Peer),%one above bottom.
    io:fwrite("reverse sync/2 got block 2\n"),

    %{BDict, BNDict, BProofTree, BlockHash} = block:check0(Block),
    Trees = block:check0(Block),
    io:fwrite("reverse sync/2 check0\n"),
    %Block2 = Block#block{trees = {BDict, BNDict, BProofTree, BlockHash}},
    %Block2 = Block#block{trees = Trees},
    %TDB = Block#block.trees,
    %Roots = block:make_roots(TDB),
    %{ok, NBlock} = talker:talk({block, Height+1}, Peer),%one above bottom.
    %Roots = NBlock#block.roots,%trees2:root_hash(NBlock#block.trees)
    Roots = Block#block.trees_hash,
    %reverse_sync2(Height, Peer, Block2, Roots).
    reverse_sync2_stream(Height, Peer, Block, Roots).

ip_url_format({{A, B, C, D}, _}) ->
    integer_to_list(A) ++ "." ++
    integer_to_list(B) ++ "." ++
    integer_to_list(C) ++ "." ++
    integer_to_list(D).

reverse_sync2_stream(Height, Peer, Block2, Roots) ->
    %PM = packer:pack({Height, 0}),
    %Peer2 =  Peer++"blocks",
    %Url = "http://" ++ ip_url_format(Peer) ++ ":8080/blocks/" ++ integer_to_list(Height-1) ++ "_0",
    Url = "http://" ++ ip_url_format(Peer) ++ ":8080/blocks/" ++ integer_to_list(Height-1) ++ "_" ++ integer_to_list(Height-101),
    io:fwrite(Url),
    io:fwrite("\n"),
    httpc:request(
      get,
      {list_to_binary(Url), []},
      [],
      [{timeout, 2000}, 
       {stream, self},
       {sync, false}]),
    receive
        {http, {_Ref, stream_start, [{"date", _}, {_, "chunked"}, {"server", "Cowboy"}]}} ->
            io:fwrite("start the stream\n"),
            rs_process_stream(Height, Block2, Roots, <<>>, Peer);
        {http, {_Ref, {{_, 404, _},_, _}}} ->
            io:fwrite("404 error\n"),
            %Trees = block:check0(Block2),
            %Block3 = Block2#block{trees = Trees},
            %reverse_sync2(Height, Peer, Block3, Roots);
            reverse_sync(Peer);
        X ->
            io:fwrite("unhandled stream header\n"),
            reverse_sync(Peer)
                %io:fwrite(X),
            %X
    after 1000 ->
            io:fwrite("failed to start receiving stream\n"),
            ok
    end.
rs_process_stream(Height, Block, Roots, Data0, Peer) ->
    %io:fwrite("rs process stream extra data " ++ integer_to_list(size(Data0))++ " " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
    go = sync_kill:status(),
    receive
        {http, {_Ref, stream, Data}} ->
            %io:fwrite("process stream, more data\n"),
            Data2 = <<Data0/binary, Data/binary>>,
            {Height2, Block2, Roots2, Data3} = 
                try_process_block(Height, Block, Roots, Data2),
               % {Height, Block, Roots, <<>>},
            timer:sleep(10),
            %io:fwrite("rs process stream " ++ integer_to_list(size(Data3)) ++ " " ++ integer_to_list(size(term_to_binary({Height2, Block2, Roots2, Data3}))) ++ " \n"),
            rs_process_stream(Height2, Block2, Roots2, Data3, Peer);
        {http, {_Ref, stream_end, _}} -> 
            io:fwrite("stream ended normally, starting next.\n"),
            if
                (Height == 0) -> ok;
                true ->
                    reverse_sync(Height, Peer)
            end;
%            <<>>;
%        {http, {_Ref, stream_start, _}} -> 
%            io:fwrite("stream start\n"),
            %rs_process_stream(Height, Block, Roots, <<>>, Peer);
%            rs_process_stream(Height, Block, Roots, Data0, Peer);
        X -> 
            io:fwrite("unhandled stream body"),
            io:fwrite(X)
    after 2000 -> 
            io:fwrite("cut off mid stream\n"),
            ok
    end.

try_process_block_helper(Block, Block2) ->
    %block 2 is the ancestor of block, we are heading towards the genesis.
    %io:fwrite(" 1.0 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
    Trees3 = block:check0(Block),
    %io:fwrite(" 1.1 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
    Block3 = Block#block{
               trees = Trees3},
    {NewDict4, _, _, ProofTree} =
        block:check3(Block2, Block3),
    %io:fwrite(" 1.2 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
    false = is_integer(ProofTree),
    block:root_hash_check(
      Block2, Block, NewDict4, ProofTree),
    ok.
  
%is root_hash_check supposed to be equivalent to trees:root_hash(block:trees_maker()) == Block#block.trees_hash 

wait_for_block(Hash, N) when N<0 -> 
    io:fwrite("checkpoint. reverse_syncing. block failed to be included in time.");
wait_for_block(Hash, N) -> 
    Step = 20,
    timer:sleep(Step),
    case block_db3:read(Hash) of
        <<>> -> wait_for_block(Hash, N-Step);
        _ -> ok
    end.
            

try_process_block(
  Height, Block, Roots, %looks like roots is unused and should be removed.
  X = <<Size:64, Data/binary>>) ->
    %io:fwrite(" 0 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
    go = sync_kill:status(),
    {ok, MTV} = application:get_env(
                  amoveo_core, minimum_to_verify),
    {ok, TestMode} = application:get_env(
                       amoveo_core, test_mode),
    F52 = forks:get(52),
    S = size(Data),
    if
        (S >= Size) -> 
            %we got another block
            %io:fwrite("got a block in checkpoint:try_process_block\n"),
            BH = block:hash(Block),
            if
                ((Height > F52) and ((Height rem 20) == 0)) or 
                ((Height rem 200) == 0) ->
                    {_, T1, T2} = erlang:timestamp(),
                    io:fwrite("absorb in reverse, streamed " ++
                                  integer_to_list(Height) ++
                                  " time: " ++
                                  integer_to_list(T1) ++
                                  " " ++
                                  integer_to_list(T2) ++
                                  "\n");
                true -> ok
            end,
            <<Blockx:(Size*8), Rest/binary>> = Data,
            Block2 = block_db3:uncompress(<<Blockx:(Size*8)>>),%block 2 is block's parent.
            %io:fwrite("checkpoint heights " ++ integer_to_list(Block2#block.height) ++ " " ++ integer_to_list(Block#block.height) ++ "\n"),
            true = (Block2#block.height + 1 == Block#block.height),
            if
                (Block#block.height == F52) -> 
                    %hardcode verkle update block hash, so we don't have to check blocks from before this point.
                    true = BH == <<185,59,27,106,59,121,158,59,113,186,179,200,161,70,238, 229,35,162,169,31,168,11,112,101,135,49,179,32,111,90,87,192>>;
                true -> ok
            end,
            %io:fwrite(" 1 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
            %spawn(fun() ->
            if
                (TestMode or ((Height > F52) and (Height > MTV))) ->
                    %timer:sleep(500);
                    try_process_block_helper(Block, Block2);


%                    Trees3 = block:check0(Block),
%                    io:fwrite(" 1.0 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
%                    Block3 = Block#block{
%                               trees = Trees3},
%                    {NewDict4, _, _, ProofTree} =
%                        block:check3(Block2, Block3),
%                    io:fwrite(" 1.1 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
%                    false = is_integer(ProofTree),
%                    block:root_hash_check(
%                      Block2, Block, NewDict4, ProofTree),
%                    ok;
                    true -> ok
                end,
            %io:fwrite(" 2 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
            {ok, Header} = headers:read(BH),
            true = (Header#header.height == 
                        Block#block.height),
            %io:fwrite("absorbing the header"),
            headers:absorb_with_block([Header]),
            %io:fwrite("absorbing the block"),
            block_db3:write(Block, BH),
            %io:fwrite("setting the top"),
            block_db3:set_top(BH),
            %io:fwrite(" 3 try process block system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
            %io:fwrite("done"),
            if
                (Height == 2) ->
                    Hash2 = block:hash(Block2),
                    {ok, Header1} = headers:read(Hash2),
                    headers:absorb_with_block([Header1]),
                    block_db3:write(Block2, Hash2),
                    block_db3:set_top(Hash2),
                    io:fwrite("synced to genesis");
                true -> ok
            end,

            {Height-1, Block2, Block#block.roots, Rest};
        true ->
            {Height, Block, Roots, X}
    end;
try_process_block(Height, Block, Roots, SmallBinary) ->
    {Height, Block, Roots, SmallBinary}.
            
tree_types_by_height(Height) -> 
    F10 = forks:get(10),%1 to 2
    F32 = forks:get(32),%2 to 3
    F35 = forks:get(35),%3 to 4
%    F44 = forks:get(44),
    F48 = forks:get(48),%4 to 5
%    F52 = forks:get(52),%to verkle
    if
        Height =< F10 ->
            [accounts, channels, existence, oracles, governance];
        Height =< F32 ->
            [accounts, channels, existence, oracles, governance, matched, unmatched];
        Height =< F35 ->
            [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades];
        Height =< F48 ->
            [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets];
        true ->
            [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets, receipts, stablecoins]
    end.

tree_types(trees5) -> [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets, receipts, stablecoins];
tree_types(trees4) -> [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets];
tree_types(trees3) -> [accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades];
tree_types(trees2) -> [accounts, channels, existence, oracles, governance, matched, unmatched];
tree_types(trees) -> [accounts, channels, existence, oracles, governance].
verify_blocks(B, _, Roots, 0) -> {true, B, Roots};
verify_blocks(B, %current block we are working on, heading towards genesis.
              P, %dictionary of blocks
              PrevRoots, 
              N) -> 
    %timer:sleep(30),
    Height = B#block.height,
    HeightCheck = (Height > (block:bottom() - 500)),
    if
        not(HeightCheck) ->
            io:fwrite("failed to sync blocks in reverse."),
            1=2;
        true -> ok
    end,
    %io:fwrite("verify blocks first "),
    %io:fwrite(integer_to_list(Height)),
    %io:fwrite("\n"),
    NB02 = dict:find(B#block.prev_hash, P),
    SS = sync:status(),
    if
        (SS == stop) -> ok;
        (Height < 1) -> 
            Genesis = block:get_by_height(0),
            NewRoots = [],
            {true, Genesis, NewRoots};
        (NB02 == error) ->
            Blocks2 = sync:low_to_high(sync:dict_to_blocks(dict:fetch_keys(P), P)),
            HeightNow = B#block.height,
            BatchStarts2 = (hd(Blocks2))#block.height,
            if
                (HeightNow == BatchStarts2) ->
                    {true, B, PrevRoots};
                true ->
                    io:fwrite({"checkpoint, can't find prev hash\n",
                               {height, HeightNow},
                               {batch_starts, BatchStarts2},
                               {more, N},
                               {keys, dict:fetch_keys(P)}})
            end;
        true ->
    {ok, MTV} = application:get_env(
                  amoveo_core, minimum_to_verify),
    {ok, TestMode} = application:get_env(
                       amoveo_core, test_mode),
            F52 = forks:get(52),
    if
        
        ((Height > F52) and ((Height rem 20) == 0)) or 
        ((Height rem 200) == 0) ->
        %((Height rem 1) == 0) ->
            {_, T1, T2} = erlang:timestamp(),
            io:fwrite("absorb in reverse in pages " ++
                          integer_to_list(B#block.height) ++
                          " time: " ++
                          integer_to_list(T1) ++
                          " " ++
                          integer_to_list(T2) ++
                          "\n");
        true -> ok
    end,
    %{ok, NB0} = dict:find(B#block.prev_hash, P),
    NB0 = case dict:find(B#block.prev_hash, P) of
              error -> 
                  Blocks = sync:low_to_high(sync:dict_to_blocks(dict:fetch_keys(P), P)),
                  %HeightNow = B#block.height,
                  BatchStarts = (hd(Blocks))#block.height,
                  io:fwrite({"checkpoint, can't find prev hash\n", 
                             {height, B#block.height}, 
                             {batch_starts, (hd(Blocks))#block.height}, 
                             {batch_ends, (hd(lists:reverse(Blocks)))#block.height},
                             {more, N},
                             {keys, dict:fetch_keys(P)}});
              {ok, NB01} -> NB01
          end,
    F52 = forks:get(52),
            BH = block:hash(B),
    if
        %((not TestMode) and (Height < MTV)) -> 
        ((Height < MTV)) -> 
            %If you don't verify that the block has the correct block hash before storing it, then you run the risk of storing incorrect data.
            %As long as the human verifies that one recent block is correct, then all the blocks that came before must be correct.
            {ok, Header} = headers:read(BH),
            true = (Header#header.height == 
                        B#block.height),
            block_db3:write(B#block{trees=0}, BH),
            block_db3:set_top(BH),
            verify_blocks(
              NB0, P, B#block.roots, N-1);
        (Height > F52) ->
            %after verkle update.
            {NewDict4, _, _, ProofTree} = 
                block:check3(NB0, B),
            false = is_integer(ProofTree),
            block:root_hash_check(
              NB0, B, NewDict4, ProofTree),
            {NDict, NNewDict, NProofTree, Hash} = 
                block:check0(NB0),
            NB2 = NB0#block{
                    trees = {NDict, NNewDict, 
                             NProofTree, Hash}},
            block_db3:write(B#block{trees=0}, BH),
            block_db3:set_top(BH),
            %verify_blocks(NB2, P, 0, N-1);
            verify_blocks(NB2, P, B#block.roots, N-1);
        true ->
            NB = NB0,

            Proof = B#block.proofs,
    %io:fwrite("verify blocks "),
    %io:fwrite(NB#block.trees),
    %io:fwrite("\n"),
            {CRM, Leaves} = 
                if
                    ((not TestMode) and (B#block.height < 38700)) -> 
                        {true, []};
                    ((not TestMode) and (B#block.height < 109000)) -> 
                        {_, _NewDict3, _, _} = block:check3(NB, B),
                        {true, []};
                    true ->
                        {_, NewDict3, _, _} = block:check3(NB, B),
                        %TreeTypes = tree_types(element(1, NB0#block.trees)),
                        TreeTypes = tree_types_by_height(NB0#block.height),
                        
                        {RootsList, Leaves0} = calc_roots2(TreeTypes, Proof, dict:fetch_keys(NewDict3), NewDict3, [], []),
                                                %Roots2 = [roots2|RootsList],
                        if
                            Height == F52 -> ok;
                            (PrevRoots == 0) -> 
                                io:fwrite({PrevRoots, RootsList});
                            not(is_tuple(PrevRoots)) ->
                                io:fwrite({PrevRoots, RootsList});
                            true -> ok
                        end,
                        Bool = case Height of
                                   F52 -> 
                                       B52_hash = block:hash(B),
                                       B52_hash == <<185,59,27,106,59,121,158,59,113,186,179,200,161,70,238, 229,35,162,169,31,168,11,112,101,135,49,179,32,111,90,87,192>>;
                                   _ ->
                                       check_roots_match(
                                         RootsList, 
                                         tl(tuple_to_list(PrevRoots)))
                               end,
                        if
                            not(Bool) -> 
                                io:fwrite({{height, Height}, {lengths, length(RootsList), length(tl(tuple_to_list(PrevRoots))), length(TreeTypes)}, {roots_list, RootsList}, {prev_roots, PrevRoots}, {tree_types, TreeTypes}, lists:map(fun(X) -> {X, dict:fetch(X, NewDict3)} end, dict:fetch_keys(NewDict3))});
                            true -> ok
                        end,
                        {Bool, Leaves0}
                end,
            {NDict, NNewDict, NProofTree, Hash} = block:check0(NB0),
            NB2 = NB0#block{trees = {NDict, NNewDict, NProofTree, Hash}},
            block_db3:write(B#block{trees = 0}, BH),
            block_db3:set_top(BH),
            verify_blocks(NB2, P, B#block.roots, N-1)
    end
    end.
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
    Cutoff = THHeight - (mft() * 2),
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
%chunkify2(R0, F, N) when (is_binary(R0) and (size(R0) > 1048576))-> 
%    io:fwrite("chunkify2 " ++ integer_to_list(N) ++ " \n"),
%    <<S:8388608/binary, R/binary>> = R0,
%    file:write_file(F++chunk_name(N), S),
%    chunkify2(R, F, N+1);
chunkify2(<<S:1048576/binary, R/binary>>, F, N) -> 
    %8388608 is 1 megabyte.
    %io:fwrite("chunkify2 " ++ integer_to_list(N) ++ " \n"),
    file:write_file(F++chunk_name(N), S),
    chunkify2(R, F, N+1);
chunkify2(R, F, N) -> 
    io:fwrite("chunkify done\n"),
    file:write_file(F ++ chunk_name(N), R),
    ok;
chunkify2(R, _, _) -> 
    io:fwrite("chunkify failed \n"),
    io:fwrite({is_binary(R), size(R), is_bitstring(R)}),
    ok.
chunk_name(N) ->
    "/" ++ integer_to_list(N) ++ 
        ".checkpoint.chunk".
backup_p(Header) ->
    H = Header#header.height,
    %<<H:256>> = block:hash(Header),
    MFT = mft(),
    if
        (MFT < 0) -> false;
        true ->
            B = (H rem mft()),
            B == 0
    end.
    
cp(CR, H, S) ->
    Name = atom_to_list(H)++S,
    os:cmd("cp " ++ CR ++ "data/" ++ Name ++ " " 
           ++ CR ++ "backup_temp/" ++ Name).
backup_trees([], _) -> 
    ok;
backup_trees([H|T], CR) -> 
    %the merkle version.
    %gen_server:call({global, ids:main_id(H)}, fail),
    trie:quick_save(H),
    cp(CR, H, ".db"),
    cp(CR, H, "_rest.db"),
    backup_trees(T, CR).


scan_blocks() ->
    H = block:height(),
    H = api:height(),
    0 = block:bottom(),
    T = block:top(),
    H = T#block.height,
    Hash = T#block.prev_hash,
    spawn(fun() ->
                  scan_blocks2(Hash)
          end).
scan_blocks2(Hash) ->
    go = sync_kill:status(),
    Block = block:get_by_hash(Hash),
    Height = Block#block.height,
    case Height of
        0 -> io:fwrite("scan_blocks: we have all the blocks.\n"),
             success;
        _ ->
            H2 = Height rem 1000,
            if
                (H2 == 0) -> 
                    io:fwrite("scan_blocks scanned to height: "),
                    io:fwrite(integer_to_list(Height)),
                    io:fwrite("\n");
                true -> ok
            end,
            Hash2 = Block#block.prev_hash,
            scan_blocks2(Hash2)
    end.



make() -> make(false).
make(Force) ->
    gen_server:call(?MODULE, {make, Force}, 100000).
    
clean() ->
    gen_server:cast(?MODULE, clean).

filtered(L) ->
    lists:filter(fun(Hash) ->
                         %only share hashes for blocks that are on the longest chain of headers.
                         {ok, H} = headers:read(Hash),
                         B = block:get_by_height(H#header.height),
                         case B of
                             empty -> false;
                             _ ->
                                 Hash2 = block:hash(B),
                                 Hash2 == Hash
                         end
                 end, L).
    
    
recent() ->
    L = gen_server:call(?MODULE, recent),
    filtered(L).



mem_check() ->
    Pids = erlang:processes(),
    R = lists:map(fun(P) ->
                          {element(2, erlang:process_info(P, current_function)),
                           erlang:process_info(P, memory),
                           erlang:process_info(P, message_queue_len)}
                  end, Pids),
    R2 = lists:sort(fun({_, {_, _}, A}, {_, {_, _}, B}) ->
                            A > B end, R),
    [R01, R02, R03, R04, R05|_] = R2,
    [R01, R02, R03, R04, R05].
    

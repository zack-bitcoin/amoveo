-module(checkpoint).
-behaviour(gen_server).
-export([backup_p/1,
         make/0, %check if the top header with a block needs a checkpoint, and if so, makes it.
         recent/0, %returns a list of recent block hashes where the block is a checkpoint.
         clean/0, %deletes old unneeded checkpoints.
         chunk_name/1,
         test/0,
         start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

%Eventually we will need a function that can recombine the chunks, and unencode the gzipped tar to restore from a checkpoint.


-include("records.hrl").

-define(LOC, constants:checkpoints()).
-record(d, {
          checkpoint_hashes = []
         }).
-define(MFT, 1000).%max fork tolerance
%ideally this should be a configuration variable that defaults as like, 3x higher than the fork_tolerance value.

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
handle_cast(make, X) -> 
    Header = headers:top_with_block(),
    B = backup_p(Header),%check if this is a checkpoint block.
    X2 = if
             B ->
                 Hash = block:hash(Header),
                 T = amoveo_sup:trees(),
                 CR = constants:custom_root(),
                 Tarball = CR ++ "backup.tar.gz",
                 Temp = CR ++ "backup_temp",
                 Temp2 = Temp ++ "2",
                 make_temp_dir(Temp),
                 make_temp_dir(Temp2),
                 ok = backup_trees(T, CR),%makes a copy of the tree files.
                 make_tarball(Tarball, Temp),
                 timer:sleep(200),
                 chunkify(Tarball, Temp2),%break up the gzipped tar file into 1 megabyte chunks, each in a different file.
                 remove_tarball(Tarball),%delete the gzipped file
                 move_chunks(Temp2, CR, Hash),
                 spawn(fun() ->
                               clean(),
                               clean()
                       end),
                 os:cmd("rm -rf "++Temp),
                 CH2 = [Hash|
                        X#d.checkpoint_hashes],
                 X#d{checkpoint_hashes = CH2};
             true -> X
         end,
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
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
   
get_chunks(Hash, IP, Port, N) ->
    case talker:talk({checkpoint, Hash, N}, IP, Port) of
        {ok, D} -> 
            R = get_chunks(Hash, IP, Port, N+1),
            <<D/binary, R/binary>>;
        {error, "out of bounds"} ->
            <<>>;
        {error, E} ->
            io:fwrite("get_chunks unknown error\n"),
            io:fwrite(E)
    end.
test() -> 
    CR = constants:custom_root(),
    IP = {127,0,0,1},
    Port = 3010,
    {ok, CPL} = talker:talk({checkpoint}, IP, Port),
    CP1 = hd(CPL),%TODO, we should take the first checkpoint that is earlier than (top height) - (fork tolerance).
    TarballData = get_chunks(CP1, IP, Port, 0),
    Tarball = CR ++ "backup.tar.gz",
    file:write_file(Tarball, TarballData),
    Temp = CR ++ "backup_temp",
    os:cmd("tar -xf " ++ Tarball),
    %now we have the database files out of the tarball, ready to be read by the merkel tree file.
    ok.
    

clean_helper([]) -> [];
clean_helper([X]) -> [X];
clean_helper(L) -> 
    L2 = lists:reverse(L),
    [CP1|[CP2|T]] = L2,
    TopHeader = headers:top_with_block(),
    THHeight = TopHeader#header.height,
    Cutoff = THHeight - ?MFT,
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
    B = (H rem ?MFT),
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
    cp(CR, H, "_leaf.db"),
    cp(CR, H, "_leaf_rest.db"),
    cp(CR, H, "_stem.db"),
    cp(CR, H, "_stem_rest.db"),
    backup_trees(T, CR).


make() ->
    gen_server:cast(?MODULE, make).
    
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

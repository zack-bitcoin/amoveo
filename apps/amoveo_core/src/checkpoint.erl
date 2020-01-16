-module(checkpoint).
-behaviour(gen_server).
-export([backup_p/1,
         make/0, recent/0, clean/0,
         start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

%Eventually we will need a function that can recombine the chunks, and unencode the gzipped tar to restore from a checkpoint.

-include("records.hrl").

-define(LOC, constants:checkpoints()).
-record(d, {
          checkpoint_hashes = []
         }).
-define(MFT, 1000).%max fork tolerance

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
                 os:cmd("mkdir " ++ CR ++ "backup_temp"),%make temp file for backing up trees, also tells all the tree processes to make a backup on the hard drive of their current state.
                 ok = backup_trees(T, CR),%makes a copy of the tree files.
                 os:cmd("tar -czvf " ++ CR ++ "backup.tar.gz " ++ CR ++ "backup_temp"),%make a gzipped tar of the copy of the tree files. 
                 os:cmd("rm " ++ CR ++ "backup_temp/*"),%delete the copy of the trees.
                 chunkify(CR ++ "backup.tar.gz", CR ++ "backup_temp/"),%break up the gzipped tar file into 1 megabyte chunks, each in a different file.
                 os:cmd("rm " ++ CR ++ "backup.tar.gz"),%delete the gzipped file
                 Encoded = base58:binary_to_base58(Hash),
                 os:cmd("mv " ++ CR ++ "backup_temp " ++ CR ++ "checkpoints/" ++ Encoded), %keep the chunks in a new folder in the checkpoints folder.
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

%test() ->
%    api:mine_block(),
%    backup_p(block:top()).

clean_helper([]) -> [];
clean_helper([X]) -> [X];
clean_helper(L) -> 
    L2 = lists:reverse(L),
    [CP1|[CP2|T]] = L2,
    TopHeader = headers:top_with_block(),
    Cutoff = TopHeader#header.height - ?MFT,
    {ok, H2} = headers:read(CP2),
    H2Height = H2#header.height,
    L3 = if
             H2Height < Cutoff ->
                 Encoded = base58:binary_to_base58(CP1),
                 CR = constants:custom_root(),
                 os:cmd("rm -r " ++ CR ++ "checkpoints/"++Encoded),
                 [CP2|T];
             true ->
                 [CP1|[CP2|T]]
         end,
    lists:reverse(L3).
chunkify(File, Folder) ->
    {ok, D} = file:read_file(File),
    chunkify2(D, Folder, 0).
chunkify2(<<>>, _, _) -> ok;
chunkify2(<<S:8388608, R/binary>>, F, N) -> 
    %8388608 is 1 megabyte.
    file:write_file(F++integer_to_list(N)++".checkpoint.chunk", <<S:8388608>>),
    chunkify2(R, F, N+1);
chunkify2(R, F, N) -> 
    file:write_file(F++integer_to_list(N)++".checkpoint.chunk", R),
    ok.
backup_p(Header) ->
    %also accepts blocks as input, but headers are much faster.
    <<H:256>> = block:hash(Header),
    B = (H rem ?MFT),%maybe instead of 1000, it should be a number that is inversely related to the governance block time. If blocks take half as long, then a fork could be twice as many blocks for the same amount of time.
    B == 0.
cp(CR, H, S) ->
    Name = atom_to_list(H)++S,
    os:cmd("cp " ++ CR ++ Name ++ " " ++ CR ++ "backup_temp/" ++ Name).
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
    %check if the top header with a block needs a checkpoint, and if so, makes it.
    gen_server:cast(?MODULE, make).
clean() ->
    %deletes old unneeded checkpoints.
    gen_server:cast(?MODULE, clean).
    
recent() ->
    %returns a list of recent block hashes where the block is a checkpoint.
    gen_server:call(?MODULE, recent).

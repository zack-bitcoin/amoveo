-module(amoveo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    application:start(inets),
    inets:start(),
    make_basic_folders(),
    make_block_folders(),
    sync:cron(),
    push_block:cron(),
    io:fwrite("starting node\n"),

    amoveo_sup:start_link().


stop(_State) ->
    io:fwrite("stopping node\n"),
    api:off().

make_basic_folders() ->
    X = ["", "blocks", "channels", "data", "keys", "oracle_questions"],
    CR = constants:custom_root(),
    mba(X, CR).
mba([], _) -> ok;
mba([H|T], CR) ->
    io:fwrite("\n\n"),
    io:fwrite(CR ++ H),
    io:fwrite("\n\n"),
    spawn(fun() ->
                  os:cmd("mkdir " ++ CR ++ H) 
          end),
    mba(T, CR).
    %read files config value
make_block_folders() ->
    mbf(0).
mbf(256) -> ok;
mbf(N) ->
    Code = blocks,
    H = to_hex(<<N>>),
    Dir = file_dir(Code),
    os:cmd("mkdir "++Dir++H),
    mbf(N+1).
file_dir(blocks) -> constants:blocks_file().

to_hex(<<>>) ->  [];
to_hex(<<A:4, B/bitstring>>) ->
    if
	A < 10 -> [(A+48)|to_hex(B)];
	true -> [(A+87)|to_hex(B)]
    end.

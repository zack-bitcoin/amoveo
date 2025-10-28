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
    pull_headers_cron(),
    R = amoveo_sup:start_link(),
    io:fwrite("starting node\n"),
    spawn(fun() ->
                  timer:sleep(4000),
                  block:height(),%to make sure we wait long enough.
                  block_hashes:second_chance(),
                  io:fwrite("attempting to sync\n"),
                  sync:start()
          end),
    mining_pool_refresher:cron(),
    R.


stop(_State) ->
    io:fwrite("stopping node\n").

make_basic_folders() ->
    X = ["", "blocks", "channels", "data", "keys", "oracle_questions", "checkpoints", "cleaner"],
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

pull_headers_cron() ->
    spawn(fun() ->
                  timer:sleep(5000),
                  pull_headers_cron2(),
                  pull_headers_cron()
          end).

pull_headers_cron2() ->
    spawn(fun() ->
                  case application:get_env(amoveo_core, pools) of
                      {ok, Pools} ->
                          Pool = hd(sync:shuffle(Pools)),
                          {_, L} = erlang:process_info(headers:process_id(), message_queue_len),
                          if
                              (L < 1) ->
                                  sync:get_headers(Pool);
                              true -> ok
                          end;
                      _ -> ok
                  end
          end).

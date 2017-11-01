-module(sync).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	start/0, stop/0, status/0, sync3/0]).
init(ok) -> {ok, stop}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(doit, go) -> 
    spawn(fun() ->
		  sync3(),
		  gen_server:cast(?MODULE, doit) end),
    {noreply, go};
handle_cast(start, _) -> 
    {noreply, go};
handle_cast(stop, _) -> 
    {noreply, stop};
handle_cast(_, X) -> {noreply, X}.
handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.

status() ->
    gen_server:call(?MODULE, status).
stop() ->
    gen_server:cast(?MODULE, stop).
start() ->
    download_blocks:sync_all(peers:all(), height()).
    %gen_server:cast(?MODULE, start),
    %timer:sleep(20),
    %gen_server:cast(?MODULE, doit).

height() -> block:height().
    %block:height(block:get_by_hash(headers:top())).

sync3() -> sync3(false).
sync3(B) ->
    Height = height(),
    download_blocks:sync_all(peers:all(), Height),
    sync2(Height, 600, B).
sync2(_Height, 0, B) -> 
    if
	B -> io:fwrite("finished syncing\n");
	true -> ok
    end,
    ok;
sync2(Height, N, _) ->
    timer:sleep(200),
    Block = block:top(),
    %Block = block:get_by_hash(headers:top()),
    io:fwrite(packer:pack(Block)),
    io:fwrite("\n"),
    B = element(1, Block) == block,
    case B of
        true -> ok;
        false ->
            io:fwrite("B \n"),
            io:fwrite(packer:pack(B)),
            io:fwrite("\n"),
            1=2
    end,
    %Height2 = block:height(block:get_by_hash(headers:top())),%maybe this should be simplified? or maybe we want to crash here?
    Height2 = height(),
    {ok, DownloadBlocksBatch} = application:get_env(ae_core, download_blocks_batch),
    Height3 = Height + DownloadBlocksBatch - 1,
    if
	Height2 > Height3 ->
	    sync3(true);
	(Height2 > Height) and (N == 1) -> 
	    sync3(true);
	true -> sync2(Height, N-1, B)
   end. 

-module(block_reader).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	doit/2]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Many, N}, _From, X) -> 
     B = many_blocks(Many, N),
    {reply, B, X};
handle_call(_, _From, X) -> {reply, X, X}.

doit(Many, N) ->
    gen_server:call(?MODULE, {read, Many, N}).

many_blocks(M, _) when M < 1 -> [];
many_blocks(Many, N) ->
    H = block:height(),
    if N > H -> [];
       true ->
            B = block:get_by_height(N),
            case B of
                empty -> many_blocks(Many-1, N+1);
                _ ->
                    [B|many_blocks(Many-1, N+1)]
            end
    end.

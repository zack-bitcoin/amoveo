%This maintains a list of node who we allow to get information from us as frequently as we can respond. 

-module(white_list).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
        check/1]).
init(ok) -> 
    {ok, IPs} = application:get_env(amoveo_core, white_list),
    DB = lists:foldl(fun(IP, Acc) -> sets:add_element(IP, Acc) end, sets:new(), IPs),
    {ok, DB}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({check, A}, _From, X) -> 
    B = sets:is_element(A, X),
    {reply, B, X}.

check(X) ->
    gen_server:call(?MODULE, {check, X}).

-module(request_frequency).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	doit/1]).
-record(freq, {time, many}).
-define(LIMIT, 100).%requests per second per IP
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(IP, _From, X) -> 
    case dict:find(IP, X) of
	error -> 
	    NF = #freq{time = erlang:timestamp(), 
		       many = 0},
	    X2 = dict:store(IP, NF, X),
	    {reply, ok, X2};
	{ok, Val} ->
	    if
		Val#freq.many > ?LIMIT ->
		    {reply, bad, X};
		true ->
		    T = timer:now_diff(erlang:timestamp(), 
				       Val#freq.time),
		    Time = T * ?LIMIT div 10000000,
		    V2 = #freq{time = erlang:timestamp(),
			       many = Val#freq.many - Time + 1},
		    X2 = dict:store(IP, V2, X),
		    {reply, ok, X2}
	    end
    end.
doit(IP) ->
    gen_server:call(?MODULE, IP).

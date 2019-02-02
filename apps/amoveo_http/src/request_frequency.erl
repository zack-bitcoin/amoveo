-module(request_frequency).
% This module is for preventing any ip address from trying to contact this node too frequently.

-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	doit/1]).
-record(freq, {time, many}).
init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(IP, _From, X) -> 
    B = white_list:check(IP),
    DF = dict:find(IP, X),
    if
        B -> {reply, ok, X};
        DF == error ->
	    NF = #freq{time = erlang:timestamp(), 
		       many = 0},
	    X2 = dict:store(IP, NF, X),
	    {reply, ok, X2};
        true ->
            {ok, Limit0} = application:get_env(amoveo_core, request_frequency),
            Limit = Limit0 * 2,%requests per 2 seconds
            {ok, Val} = DF,
	    TimeNow = erlang:timestamp(),
	    T = timer:now_diff(TimeNow, 
			       Val#freq.time),
	    S = T / 1000000,%seconds
	    Many0 = Val#freq.many * math:pow(0.5, S), %every second, divide how many have been used up by 1/2.
	    Many = Many0 + 1,
	    V2 = #freq{time = TimeNow,
		       many = Many},
	    X2 = dict:store(IP, V2, X),
	    R = if
		Many > Limit -> 
			%io:fwrite("requesting too often: "),
			%io:fwrite(packer:pack(IP)),
			%io:fwrite(" "),
			%io:fwrite(integer_to_list(round(Many))),
			%io:fwrite("\n"),
			bad;
		true -> ok
	    end,
	    {reply, R, X2}
    end.
doit(IP) ->
    gen_server:call(?MODULE, IP).

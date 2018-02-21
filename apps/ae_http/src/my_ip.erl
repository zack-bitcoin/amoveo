
-module(my_ip).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, get/0]).
init(ok) -> {ok, empty}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call(get, _From, X) -> 
    Y = case X of 
	    empty -> my_ip();
	    _ -> X
	end,
    {reply, Y, Y};
handle_call(_, _From, X) -> {reply, X, X}.

get() -> gen_server:call(?MODULE, get).

my_ip() -> my_ip(peers:all()).
my_ip([]) -> empty;
my_ip([[A, B]|T]) ->
    my_ip([{A, B}|T]);
my_ip([P|T]) ->
    io:fwrite(packer:pack(P)),
    io:fwrite("\n"),
    case talker:talk_timeout({f}, P, 4000) of
	{ok, MyIP} ->
	    case MyIP of 
		{10, _, _, _} -> my_ip(T);
		{192, 168, _, _} -> my_ip(T);
		{172, X, _, _} -> 
		    if
			((X < 32) and (X > 15)) -> my_ip(T);
			true -> MyIP
		    end;
		{_, _, _, _} -> MyIP;
		_ -> my_ip(T)
	    end;
	X ->  my_ip(T)
	    %io:fwrite("my_ip issue \n"),
	    %io:fwrite(packer:pack(X)),
	    %io:fwrite("\n")
    end.

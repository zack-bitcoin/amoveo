-module(header_cache).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/2]).

-define(name, "header_cache.db").
-record(db, {file = ""}).

init(ok) -> 
    process_flag(trap_exit, true),
    {{ok, F}, _} = {file:open(?name, [write, read, raw, binary]), ?name},
    {ok, #db{file = F}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, #db{file = F}) -> 
    io:format("header cache died!"), 
    file:close(F),
    ok.
handle_info(_, X) -> {noreply, X}.
%handle_cast({store, N, Headers}, X) -> 
%    io:fwrite("header cache store internal\n"),
%    X2 = dict:store(N, Headers, X),
%    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
%handle_call({read, N}, _From, X) -> 
%    {reply, dict:find(N, X), X};
handle_call(_, _From, X) -> {reply, X, X}.


read(Height, Many) ->
    ok.

%read(N) ->
%    0 = N rem 5000,
%    gen_server:call(?MODULE, {read, N}).
%store(N, Headers) ->
%    0 = N rem 5000,
%    io:fwrite("attempt to store in header cache "),
%    io:fwrite(integer_to_list(N)),
%    io:fwrite("\n"),
%    gen_server:cast(?MODULE, {store, N, Headers}).


%fill() ->
%    H = headers:top(),
%    fill2(5000, H).
%fill2(Height, TopHeader) ->
%    Nth = ext_handler:get_header_by_height(Height, TopHeader),
%    Result = ext_handler:many_headers2(5000, Nth, []),
%    HH = element(2, (hd(lists:reverse(Result)))),
%    if
%        (Height == HH) ->
            %(length(Result) == 5000) ->
%            store(Height, Result),
%            fill2(Height + 5000, TopHeader);
%        true -> 
            %io:fwrite({length(Result), Result}),
%            ok
%    end.

%this should be redundant between hard drive and ram, that way we can be faster.
-module(block_pointers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/2,write/2,test/0,height/0,append/1,garbage/1,start/0,set_start/1]).
%-define(file, "block_pointers.db").
-define(file, constants:block_pointers()).
%-define(start, "pointers_start.db").
-define(start, constants:pointers_start()).
-define(word, 8).
init(ok) -> 
    process_flag(trap_exit, true),
    K = case db:read(?start) of
	"" -> 0;
	X -> X
	end,
    {ok, K}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, Start) -> 
    db:save(?start, Start),
    io:format("block pointers died!"),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_call(start, _From, Start) -> {reply, Start, Start};
handle_call({garbage, NewStart}, _From, Start) -> 
    Amount = NewStart - Start,
    true = Amount > 0,
    <<First:38, _:26>> = read_helper(Start, 1, Start),
    <<Last:38, Size:26>> = read_helper(NewStart - 1, 1, Start),
    Delete = Last+Size-First,
    shift_subtract(Amount, Delete),
    {reply, Delete, NewStart};
handle_call({write, X, N}, _From, Start) -> 
    true = N > Start-1,
    true = size(X) == ?word,
    {ok, File} = file:open(?file, [write, read, raw]),
    file:pwrite(File, (N-Start)*?word, X),
    file:close(File),
    {reply, ok, Start}.
handle_cast({set_start, N}, _) -> {noreply, N}.
set_start(N) -> gen_server:cast(?MODULE, {set_start, N}).
shift_subtract(Shift, Subtract) ->
    %T = "temp.db",
    T = constants:temp(),
    FS = filelib:file_size(?file) div ?word,
    {ok, RFile } = file:open(?file, [read, binary, raw]),
    {ok, WFile } = file:open(T, [binary, raw, write, read]),
    shift_subtract_helper(Shift, Subtract, RFile, WFile, 0, FS),
    file:close(RFile),
    file:close(WFile),
    file:copy(T, ?file),
    file:delete(T).
    
shift_subtract_helper(Shift, _, _, _, I, FS) when (I + Shift) > (FS - 1) -> 0;
shift_subtract_helper(Shift, Subtract, RFile, WFile, I, FS) ->
    {ok, <<A:38, B:26>>} = file:pread(RFile, (I+Shift)*?word, ?word),
    file:pwrite(WFile, (I)*?word, <<(A-Subtract):38, B:26>>),
    shift_subtract_helper(Shift, Subtract, RFile, WFile, I+1, FS).
write(X, N) -> 
    8 = size(X),
    gen_server:call(?MODULE, {write, X, N}).
append(X) -> write(X, height()).
start() -> gen_server:call(?MODULE, start).
read(N, Many) -> 
    Start = start(), 
    read_helper(N, Many, Start).
read_helper(N, Many, Start) ->
    if
	N < Start - 1 -> none;
	true ->
	    true = N > Start - 1,
	    {ok, File } = file:open(?file, [read, binary, raw]),
	    {ok, X} = file:pread(File, (N-Start)*?word, Many*?word),
	    file:close(File),
	    X
    end.
height() -> start() + (filelib:file_size(?file) div ?word).
garbage(NewStart) -> 
    Amount = NewStart - start(),
    if
	Amount > 0 -> gen_server:call(?MODULE, {garbage, NewStart});
	true -> 0
    end.
test() -> 
    A = round(math:pow(2, 32)) - 1,
    C = <<A:32>>,
    B = <<"ABCD">>,
    X2 = << B/binary, C/binary >>,
    write(X2, 2),
    X1 = <<"        ">>,
    X0 = <<"asdfghjk">>,
    write(X1, 1),
    write(X0, 0),
    %timer:sleep(200),
    X = << X0/binary, X1/binary, X2/binary>>,
    X = read(0, 3),
    file:delete(?file),
    success.


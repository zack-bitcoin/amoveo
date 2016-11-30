-module(block_dump).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/2,write/1,garbage/1,test/0]).
%-define(file, "blocks.db").
-define(file, constants:blocks()).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({garbage, Many}, _From, X) -> 
    %Entire file contents need to be shifted left by Many bytes.
    shift(Many),
    {reply, ok, X};
handle_call({read, Location, Bytes}, _From, X) -> 
    {ok, File} = file:open(?file, [read, binary, raw]),
    {ok, Y} = file:pread(File, Location, Bytes),
    file:close(File),
    {reply, Y, X};
handle_call({write, Bytes}, _From, X) ->
    Location = filelib:file_size(?file),
    {ok, File} = file:open(?file, [write, binary, read]),
    file:pwrite(File, Location, Bytes),
    file:close(File),
    {reply, {Location, size(Bytes)}, X}.
garbage(Many) -> 
    if
	Many > 0 -> gen_server:call(?MODULE, {garbage, Many});
	true -> 0
    end.
read(Location, Bytes) -> 
    true = Bytes > 0,
    zlib:uncompress(gen_server:call(?MODULE, {read, Location, Bytes})).
write(Bytes) -> gen_server:call(?MODULE, {write, zlib:compress(Bytes)}).
shift(Many) -> 
    %T = "temp.db",
    T = constants:temp(),
    FS = filelib:file_size(?file),
    {ok, RFile } = file:open(?file, [read, binary, raw]),
    {ok, WFile } = file:open(T, [binary, raw, write, read]),
    shift(Many, RFile, WFile, 0, FS),
    file:close(WFile),
    file:close(RFile),
    file:copy(T, ?file),
    file:delete(T).
shift(Many, RFile, WFile, I, FS) when I+Many < FS - 262145 ->
    {ok, X} = file:pread(RFile, I+Many, 262144),
    file:pwrite(WFile, I, X),
    shift(Many, RFile, WFile, I+262144, FS);
shift(Many, RFile, WFile, I, FS) when I+Many < FS - 32769 ->
    {ok, X} = file:pread(RFile, I+Many, 32769),
    file:pwrite(WFile, I, X),
    shift(Many, RFile, WFile, I+32768, FS);
shift(Many, RFile, WFile, I, FS) when I+Many < FS - 4097 ->
    {ok, X} = file:pread(RFile, I+Many, 4096),
    file:pwrite(WFile, I, X),
    shift(Many, RFile, WFile, I+4096, FS);
shift(Many, RFile, WFile, I, FS) when I+Many < FS - 513 ->
    {ok, X} = file:pread(RFile, I+Many, 512),
    file:pwrite(WFile, I, X),
    shift(Many, RFile, WFile, I+512, FS);
shift(Many, RFile, WFile, I, FS) when I+Many < FS - 65 ->
    {ok, X} = file:pread(RFile, I+Many, 64),
    file:pwrite(WFile, I, X),
    shift(Many, RFile, WFile, I+64, FS);
shift(Many, RFile, WFile, I, FS) when I+Many < FS - 9 ->
    {ok, X} = file:pread(RFile, I+Many, 8),
    file:pwrite(WFile, I, X),
    shift(Many, RFile, WFile, I+8, FS);
shift(Many, RFile, WFile, I, FS) when I+Many < FS ->
    {ok, X} = file:pread(RFile, I+Many, 1),
    file:pwrite(WFile, I, X),
    shift(Many, RFile, WFile, I+1, FS);
shift(_, _, _, _, _) -> 0.

test() ->
    T = <<0:80000>>,
    write(T),
    write(T),
    garbage(20000),
    S = <<"1234567abcdef">>,
    {X, Y} = write(S),
    {A, Y} = write(S),
    false = A == X,
    S = read(X, Y),
    S = read(A, Y),
    %garbage(54),%first block is 54 long.
    write(S),
    S = read(0, Y),
    S = read(Y, Y),
    S = read(2*Y, Y),
    file:delete(?file),
    success.

%depending on how complicated it is to compute the next top, we may have to charge an additional fee when people delete in bad spots.

%The byte array should be backed up to disk. Instead of writing the entire thing to disk at each block, we should manipulate individual bits in the file at each block. 
%If the bit is set to zero, then that address is ready to be written in.
%Top should point to the lowest known address that is deleted.
-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read_account/1,write/2,test/0,size/0,write_helper/3,top/0,delete/1,array/0,update/6,update/5,empty/0,empty/1,nonce/1,addr/1,balance/1,height/1,walk/2,unit_cost/1,reset/0]).
-define(file, constants:accounts()).
-define(empty, constants:d_accounts()).
%addr is 96 bits. balance is 48 bits. Nonce is 32 bits. delegated is 48 bits. height is 32 bits, bringing the total to 32 bytes.

-define(word, ((164 + constants:address_entropy()) div 8)).
-record(acc, {balance = 0, nonce = 0, addr = "", height = 0}).%the height when delegation fees were last payed. 
init(ok) -> 
    {T, D} = 
	case file:read_file(?empty) of
	    {error, enoent} -> 
		P = base58:base58_to_binary(binary_to_list(testnet_sign:pubkey2address(constants:master_pub()))),
		IC = constants:initial_coins(),
		Balance = IC,
		write_helper(0, <<Balance:48, 0:32, 0:32, P/binary>>, ?file),
		Top = 1,
		DeletedArray = << 1:1 , 0:7 >>,
		write_helper(0, DeletedArray, ?empty),
		{Top, DeletedArray};
	    {ok, DeletedArray} ->
		Top = walk(0, DeletedArray),
		{Top, DeletedArray}
	end,
    {ok, {T, D}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
empty() -> #acc{}.
empty(Addr) -> #acc{addr = Addr}.
unit_cost(TotalCoins) ->
    AFee = fractions:multiply_int(constants:account_fee(), TotalCoins),
    AFee.
update(Acc, H, Dbal, N, TotalCoins) ->
    update(Acc, H, Dbal, N, TotalCoins, normal).
update(Acc, H, Dbal, N, TotalCoins, Tag) ->
    true = ((N == 0) or (N == 1)),
    Gap = H - Acc#acc.height,
    UCost = unit_cost(TotalCoins),
    Cost = UCost * Gap,
    Balance = Acc#acc.balance + Dbal,
    MaxC = UCost * constants:max_reveal(),
    _ = case Tag of
	normal -> true = ((Balance - MaxC) > 0), 0;%You need enough money to stay open at least this long, so that your partner has time to close the channel before your account gets deleted.
	nocheck -> 0
    end,
    #acc{
       balance = Balance - Cost,
       nonce = Acc#acc.nonce + N,
       addr = Acc#acc.addr,
       height = H}.
nonce(Acc) -> Acc#acc.nonce.
addr(Acc) -> Acc#acc.addr.
balance(Acc) -> Acc#acc.balance.
height(Acc) -> Acc#acc.height.
write_helper(N, Val, File) ->
%since we are reading it a bunch of changes at a time for each block, there should be a way to only open the file once, make all the changes, and then close it. 
    case file:open(File, [write, read, raw]) of
        {ok, F} ->
            file:pwrite(F, N, Val),%multiplying by word is no good for empty...
            file:close(F);
        {error, _Reason} ->
            write_helper(N, Val, File)
    end.

walk(Top, Array) -> 
    << _:Top, Tail/bitstring>> = Array,
    walk_helper(Tail, Top).
walk_helper(<<>>, Counter) -> Counter;
walk_helper(<< 127:8, B/bitstring>>, Counter) -> walk_helper(B, Counter + 8);
walk_helper(<< 1:1, B/bitstring>>, Counter) -> walk_helper(B, Counter + 1);
walk_helper(<< 0:1, _B/bitstring>>, Counter) -> Counter.
handle_cast(reset, _) ->
    {ok, X} = init(ok),
    {noreply, X};
handle_cast({delete, N}, {Top, Array}) -> 
    Byte = hd(binary_to_list(read_empty(N))),
    Remove = bnot round(math:pow(2, 7 - (N rem 8))),
    NewByte = Byte band Remove,
    write_helper(N div 8, <<NewByte>>, ?empty),
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,0:1,B/bitstring>>,
    write_helper(N*?word, #acc{}, ?file),
    {noreply, {min(Top, N), NewArray}};
handle_cast({write, N, Val}, {Top, Array}) -> 
    S = size(),
    if
        N > S -> write_helper(N div 8, <<0>>, ?empty);
        true -> 0 = 0
    end,
    Byte = hd(binary_to_list(read_empty(N))),
    Remove = round(math:pow(2, 7 - (N rem 8))),
    NewByte = Byte bor Remove,
    write_helper(N div 8, <<NewByte>>, ?empty),
    <<A:N,_:1,B/bitstring>> = Array,
    NewArray = <<A:N,1:1,B/bitstring>>,
    false = N > size(),
    write_helper(N*?word, Val, ?file),
    {noreply, {walk(Top, NewArray), NewArray}}.
handle_call(array, _From, {Top, Array}) -> {reply, Array, {Top, Array}};
handle_call(top, _From, {Top, Array}) -> {reply, Top, {Top, Array}}.
top() -> gen_server:call(?MODULE, top).
array() -> gen_server:call(?MODULE, array).
delete(N) -> gen_server:cast(?MODULE, {delete, N}).
read_empty(N) -> 
    {ok, File} = file:open(?empty, [read, binary, raw]),
    case file:pread(File, N div 8, 1) of
	eof -> write_helper(N div 8, <<0>>, ?empty),
	       read_empty(N);
	{ok, X} -> file:close(File), X
    end.
read_file(N) -> 
    {ok, File} = file:open(?file, [read, binary, raw]),
    case file:pread(File, N*?word, ?word) of
	eof -> write_helper(N*?word, <<0:4000>>, ?file),% 4000 = 8*?word.
	       read_file(N);
	{error, einval} ->
	    io:fwrite("read file error \n"),
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	{ok, X} -> 
	    file:close(File), X
    end.
read_account(-1) -> 
    io:fwrite("\nError ----> use keys:update_id(_).\n");
read_account(N) -> %maybe this should be a call too, that way we can use the ram to see if it is already deleted?
    T = top(),
    if
	N >= T -> #acc{};
	true ->
	    X = read_file(N),%if this is above the end of the file, then just return an account of all zeros.
	    <<Balance:48, Nonce:32, Height: 32, P/binary>> = X,
	    Addr = list_to_binary(base58:binary_to_base58(P)),
	    #acc{balance = Balance, nonce = Nonce, addr = Addr, height = Height}
    end.
write(N, Acc) ->
    P = base58:base58_to_binary(binary_to_list(Acc#acc.addr)),
    %P = Acc#acc.addr,
    %12 = size(P),
    S = size(P),
    S = (constants:address_entropy() + 4) div 8,
    Val = << (Acc#acc.balance):48, 
             (Acc#acc.nonce):32, 
	     (Acc#acc.height):32,
             P/binary >>,
    gen_server:cast(?MODULE, {write, N, Val}).
size() -> filelib:file_size(?file) div ?word.
append(Acc) -> write(top(), Acc).
reset() -> gen_server:cast(?MODULE, reset).

test() -> 
    << 13:4 >> = << 1:1, 1:1, 0:1, 1:1 >>,%13=8+4+1
    0 = walk(0, << >>),
    0 = walk(0, << 0:1 >>),
    2 = walk(0, << 1:1, 1:1, 0:1, 1:1 >>),
    3 = walk(0, << 1:1, 1:1, 1:1, 0:1, 0:30 >>),
    1 = walk(0, << 2:2 >>),
    0 = walk(0, << 1:2 >>),
    2 = walk(0, << 24:5 >>),
    5 = walk(0, << 31:5 >>),
    5 = walk(2, << 31:5 >>),
    5 = walk(5, << 31:5 >>),
    Pub = <<"BIv7EW1vyIxwiB/9yKIiNvVydp6U/2QdSwCrLNNxC9X4CCwpwDTTDP8Z4yAx1g6Z4DzkKVpakfo7W9UdYQkmc+4=">>,
%<<"BIXotG1x5BhwxVKxjsCyrgJASovEJ5Yk/PszEdIoS/nKRNRv0P0E8RvNloMnBrFnggjV/F7pso/2PA4JDd6WQCE=">>,
    Addr = testnet_sign:pubkey2address(Pub),
    Balance = 50000000,
    A = #acc{addr = Addr, nonce = 0, balance = Balance},
    delete(3),
    delete(2),
    delete(1),
    1 = top(),
    append(A),
    2 = top(),
    append(A),
    3 = top(),
    %delete(0),
    %0 = top(),
    %append(A),
    3 = top(),
    delete(1),
    1 = top(),
    append(A),
    3 = top(),
    Acc = read_account(1),
    Acc = read_account(2),
    delete(1),
    %delete(0),
    %0 = top(),
    %append(A),
    1 = top(),
    append(A),
    3 = top(),
    Acc = read_account(1),
    Addr = Acc#acc.addr,
    Balance = Acc#acc.balance,
    delete(3),
    delete(2),
    delete(1),
    success.

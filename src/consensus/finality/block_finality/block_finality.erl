%Each block has both a height, and number. Blocks reference each other by height, and the database looks up blocks by height.
%Blocks only get appended to this once we are sure that block will be part of the blockchain.
%O(1) lookup time for blocks. Adding more blocks doesn't slow it down.
%this is made up of 3 modules. the block_pointers.db file uses 8 bytes for each block. The 8 bytes encodes the position and size of the block in block_dump.db file. block_finality.erl module gives an interface.
-module(block_finality).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,append/2,top/0,top_block/0,test/0]).
-define(word, 8).
init(ok) -> 
    H = top(),
    Genesis = testnet_sign:empty(block_tree:empty_block()),
    if
	H == 0 -> append_helper(term_to_binary(Genesis), 0);%store the genesis block into the database.
	true -> 0 = 0
    end,
    {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, D) -> {noreply, D}.
handle_call({append, X, NewHeight}, _From, D) ->% {reply, 0, X}.
    append_helper(X, NewHeight),
    {reply, 0, D}.
append_helper(X, NewHeight) ->
    NewStart = NewHeight - constants:max_reveal() - 5,
    {A, B} = block_dump:write(X),
    N = <<A:38, B:26>>,%2**26 ~60 mb is how long blocks can be, 2**38 ~250 GB is how big the blockchain can be. 38+26=64 bits is 8 bytes, so I defined word as 8.
    block_pointers:append(N),
    G = ((NewHeight rem (fractions:multiply_int(constants:backup(), constants:max_reveal()))) == 0),
    if
	G ->
	    Remove = block_pointers:garbage(NewStart),
	    block_dump:garbage(Remove);
	true -> 0
    end.
append(Block, NewHeight) -> gen_server:call(?MODULE, {append, term_to_binary(Block), NewHeight}).
top() -> block_pointers:height().
read(N) -> 
    S = block_pointers:start(),
    %true = N > S - 1,
    C = top(),
    if
	N < S - 1 -> <<"none">>;
	N < 0 -> <<"none">>;
	N > C-1 -> <<"none">>;
	true ->
	    case block_pointers:read(N, 1) of
		<<"none">> -> <<"none">>;
		<<A:38, B:26>>  ->
		    X = block_dump:read(A, B),
		    binary_to_term(X)
	    end
    end.
top_block() -> read(top()-1).
fo4(_, Start, End) when Start > End -> 0;
fo4(X, Start, End) -> 
    append(X, Start),
    fo4(X, Start+1, End).
    
test() ->
    X = {abc, 2, 3},
    S = 1000,
    fo4(X, 0, S),
    <<"none">> = read(0),
    <<"none">> = read(100),
    X = read(S-100),
    success.



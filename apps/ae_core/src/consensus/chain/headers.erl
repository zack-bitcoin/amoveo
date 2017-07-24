% This gen_server keeps track of the headers for all the blocks.
-module(headers).
-behaviour(gen_server).
%% External exports
-export([start_link/0, absorb/1, read/1, make_header/8, 
	 serialize/1, test/0,
	 prev_hash/1, height/1, time/1, version/1, trees/1, txs/1, nonce/1, difficulty/1, accumulative_difficulty/1,
	 difficulty_should_be/1, top/0, dump/0,
	 hard_set_top/1
	]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(header, {prev_hash, height, time, version, trees, txs, nonce, difficulty, accumulative_difficulty = 0}).
-record(s, {headers = dict:new(), top = #header{}}).
prev_hash(H) -> H#header.prev_hash.
height(H) -> H#header.height.
time(H) -> H#header.time.
version(H) -> H#header.version.
trees(H) -> H#header.trees.
txs(H) -> H#header.txs.
nonce(H) -> H#header.nonce.
difficulty(H) -> H#header.difficulty.
accumulative_difficulty(H) -> H#header.accumulative_difficulty.
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
    {ok, #s{}}.

handle_call({read, Hash}, _From, State) ->
    {reply, dict:find(Hash, State#s.headers), State};
handle_call({dump}, _From, _State) ->
    {reply, ok, #s{}};
handle_call({top}, _From, State) ->
    {reply, State#s.top, State};
handle_call({hard_set_top, Header}, _From, State) ->
    H = block:hash(Header),
    D = dict:store(H, Header, dict:new()),
    {reply, ok, #s{headers = D, top = Header}};
handle_call({add, Hash, Header}, _From, State) ->
    AD = Header#header.accumulative_difficulty,
    AA = (State#s.top),
    AF = AA#header.accumulative_difficulty,
    H = case AD > AF of
		 true -> Header;
		 false -> AA
	     end,
    Headers = dict:store(Hash, Header, State#s.headers),
    {reply, ok, State#s{headers = Headers, top = H}}.
%handle_call(_, _From, State) ->
%    {reply, ok, State}.
handle_cast(_, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
make_header(PH, 0, Time, Version, Trees, Txs, Nonce, Difficulty) ->
    #header{prev_hash = PH,
	    height = 0, 
	    time = Time, 
	    version = Version,
	    trees = Trees,
	    txs = Txs,
	    nonce = Nonce,
	    difficulty = Difficulty,
	    accumulative_difficulty = 0};
make_header(PH, Height, Time, Version, Trees, Txs, Nonce, Difficulty) ->
    case read(PH) of
	{ok, PrevHeader} ->
	    AC = pow:sci2int(Difficulty) + PrevHeader#header.accumulative_difficulty,
	    #header{prev_hash = PH,
	    height = Height, 
		    time = Time, 
		    version = Version,
		    trees = Trees,
		    txs = Txs,
		    nonce = Nonce,
		    difficulty = Difficulty,
		    accumulative_difficulty = AC};
	_ -> error %the parent is unknown
    end.
txs_hash(X) ->
    testnet_hasher:doit(X).
serialize(H) ->
    PH = H#header.prev_hash,
    Height = H#header.height,
    Time = H#header.time,
    Version = H#header.version,
    Trees = H#header.trees,
    Txs = H#header.txs,
    Nonce = H#header.nonce,
    Difficulty = H#header.difficulty,
    true = size(PH) == constants:hash_size(),
    true = size(Trees) == constants:hash_size(),
    true = size(Txs) == constants:hash_size(),
    <<PH/binary,
     Height:(constants:height_bits()),
     Time:(constants:time_bits()),
     Version:(constants:version_bits()),
     Trees/binary,
     Txs/binary,
     Difficulty:16,
     Nonce:(constants:hash_size()*8)
     >>.
deserialize(B) ->
    HS = constants:hash_size()*8,
    HB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    <<
     PH:HS,
     Height:HB,
     Time:TB,
     Version:VB,
     Trees:HS,
     Txs:HS,
     Difficulty:16,
     Nonce:HS
     >> = B,
    #header{prev_hash = <<PH:HS>>,
	    height = Height,
	    time = Time,
	    version = Version,
	    trees = <<Trees:HS>>,
	    txs = <<Txs:HS>>,
	    difficulty = Difficulty,
	    nonce = Nonce}.
header_size() ->	
    HS = constants:hash_size()*8,
    (HS*4) + constants:height_bits()
	+ constants:time_bits()
	+ constants:version_bits()
	+ 16.
    
check_pow(Header) ->    
    S = serialize(Header),
    W = hash:doit(S, constants:hash_size()),
    I = pow:hash2integer(W),
    I > Header#header.difficulty.
difficulty_should_be(A) ->
    D1 = A#header.difficulty,
    RF = constants:retarget_frequency(),
    Height = A#header.height,
    X = Height rem RF,
    if
	X == 0 and not(Height < 10)->
	    check_difficulty2(A);
	true ->
	    D1
    end.
check_difficulty(A) ->
    B = if
	    A#header.height < 2 -> constants:initial_difficulty();
	    true ->
		{ok, PHeader} = read(A#header.prev_hash),
		difficulty_should_be(PHeader)
	end,
    {B == A#header.difficulty, B}.
median(L) ->
    S = length(L),
    F = fun(A, B) -> A > B end,
    Sorted = lists:sort(F, L),
    lists:nth(S div 2, Sorted).
check_difficulty2(Header) ->
    F = constants:retarget_frequency() div 2,
    {Times1, Hash2000} = retarget(Header, F, []),
    {Times2, _} = retarget(Hash2000, F, []),
    M1 = median(Times1),
    M2 = median(Times2),
    Tbig = M1 - M2,
    T = Tbig div F,
    NT = pow:recalculate(Header#header.difficulty, 
			 constants:block_time(),
			 max(1, T)),
    max(NT, constants:initial_difficulty()).
retarget(Header, 1, L) -> {L, Header};
retarget(Header, N, L) ->
    {ok, PH} = read(Header#header.prev_hash),
    T = PH#header.time,
    retarget(PH, N-1, [T|L]).
    

absorb([]) -> ok;
absorb([First|T]) when is_binary(First) ->
    A = deserialize(First),
    absorb([A|T]);
absorb([A|T]) ->
    true = A#header.difficulty >= constants:initial_difficulty(),
    Hash = block:hash(A),
    case read(Hash) of 
	{ok, _} -> 
	    io:fwrite("absorb header repeat\n"),
	    ok; %don't store the same header more than once.
	error ->
	    true = check_pow(A),%check that there is enough pow for the difficulty written on the block
	    N = A#header.height > 1,
	    {true, _} = check_difficulty(A),%check that the difficulty written on the block is correctly calculated
	    ok = gen_server:call(?MODULE, {add, Hash, A}),
	    file:write_file(constants:headers_file(), serialize(A), [append]) %we keep all the good headers we know about in the order we learned about them. This is used for sharing the entire history of headers quickly.
    end,
    absorb(T).
hard_set_top(Header) ->
    gen_server:call(?MODULE, {hard_set_top, Header}).
accumulate_diff(Diff, PrevHeader) ->
    %Hash = block:hash(PrevHeader),
    %GB = block:read_int(0, Hash),
    %GH = block:block_to_header(GB),
    %if
	%PrevHeader == GH -> 0;
	%true ->
    pow:sci2int(Diff),
    + PrevHeader#header.accumulative_difficulty.
    %end.
    

read(Hash) ->
    gen_server:call(?MODULE, {read, Hash}).
dump() ->    
    gen_server:call(?MODULE, {dump}).
top() ->    
    gen_server:call(?MODULE, {top}).
	    
test() ->
    H = testnet_hasher:doit(<<>>),
    Header = setelement(10, make_header(H, 0, 0, 0, H, H, 0, 0), undefined),
    Header = deserialize(serialize(Header)),
    absorb([Header]),
    H1 = testnet_hasher:doit(serialize(Header)),
    Header2 = setelement(10, make_header(H1, 0, 0, 0, H, H, 0, 0), undefined),
    absorb([Header2]),
    H1 = block:hash(Header),
    {ok, Header} = read(H1).
    


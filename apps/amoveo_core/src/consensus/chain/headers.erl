-module(headers).
-behaviour(gen_server).
-export([absorb/1, absorb_with_block/1, read/1, top/0, dump/0, top_with_block/0,
         make_header/9, serialize/1, deserialize/1,
         difficulty_should_be/1, test/0]).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("../../records.hrl").
-define(LOC, constants:headers_file()).
-record(s, {headers = dict:new(),
            top = #header{},
	    top_with_block = #header{}
	   }).
init([]) ->
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    K = if
	    X == "" -> 
                empty_data();
	    true -> X
	end,
    {ok, K}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
handle_call({read, Hash}, _From, State) ->
    {reply, dict:find(Hash, State#s.headers), State};
handle_call({check}, _From, State) ->
    {reply, State, State};
handle_call({dump}, _From, _State) ->
    {reply, ok, empty_data()};
handle_call({top_with_block}, _From, State) ->
    {reply, State#s.top_with_block, State};
handle_call({top}, _From, State) ->
    {reply, State#s.top, State};
handle_call({add_with_block, Hash, Header}, _From, State) ->
    AD = Header#header.accumulative_difficulty,
    Top = State#s.top_with_block,
    AF = Top#header.accumulative_difficulty,
    NewTop = case AD >= AF of
                 true -> 
		     found_block_timer:add(),
		     Header;
                 false -> Top
        end,
    %Headers = dict:store(Hash, Header, State#s.headers),
    {reply, ok, State#s{top_with_block = NewTop}};
handle_call({add, Hash, Header}, _From, State) ->
    AD = Header#header.accumulative_difficulty,
    Top = State#s.top,
    AF = Top#header.accumulative_difficulty,
    NewTop = case AD >= AF of
                 true -> Header;
                 false -> Top
        end,
    Headers = dict:store(Hash, Header, State#s.headers),
    {reply, ok, State#s{headers = Headers, top = NewTop}}.
handle_cast(_, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, X) ->
    db:save(?LOC, X),
    io:fwrite("headers died!\n"),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check() -> gen_server:call(?MODULE, {check}).

absorb_with_block([]) -> 0;
absorb_with_block([F|T]) when is_binary(F) ->
    absorb([deserialize(F)|T]);
absorb_with_block([F|T]) ->
    Hash = block:hash(F),
    %false = empty == block:get_by_hash(Hash),
    ok = gen_server:call(?MODULE, {add_with_block, Hash, F}),
    absorb_with_block(T).
absorb(X) -> 
    absorb(X, block:hash(block:get_by_height(0))).
absorb([], CommonHash) -> 
    CommonHash;
absorb([First|T], R) when is_binary(First) ->
    A = deserialize(First),
    absorb([A|T], R);
absorb([Header | T], CommonHash) ->
    true = Header#header.difficulty >= constants:initial_difficulty(),
    Hash = block:hash(Header),
    case read(Hash) of
        {ok, _} -> 
	    absorb(T, Hash); %don't store the same header more than once.
        error ->
            true = check_pow(Header),%check that there is enough pow for the difficulty written on the block
            {true, _} = check_difficulty(Header),%check that the difficulty written on the block is correctly calculated
            ok = gen_server:call(?MODULE, {add, Hash, Header}),
            absorb(T, CommonHash)
    end.
check_pow(Header) ->
    MineDiff = Header#header.difficulty,
    Data = block:hash(Header#header{nonce = <<0:256>>}),
    <<Nonce:256>> = Header#header.nonce,
    pow:check_pow({pow, Data, MineDiff, Nonce}, constants:hash_size()).

check_difficulty(A) ->
    B = case A#header.height < 2 of
            true ->
                constants:initial_difficulty();
            false ->
                {ok, PHeader} = read(A#header.prev_hash),
                difficulty_should_be(PHeader)
        end,
    {B == A#header.difficulty, B}.
read(Hash) -> gen_server:call(?MODULE, {read, Hash}).
top() -> gen_server:call(?MODULE, {top}).
top_with_block() -> gen_server:call(?MODULE, {top_with_block}).
dump() -> gen_server:call(?MODULE, {dump}).
make_header(PH, 0, Time, Version, TreesHash, TxsProofHash, Nonce, Difficulty, Period) ->
    #header{prev_hash = PH,
	    height = 0, 
	    time = Time, 
	    version = Version,
	    trees_hash = TreesHash,
	    txs_proof_hash = TxsProofHash,
	    nonce = <<Nonce:256>>,
	    difficulty = Difficulty,
	    accumulative_difficulty = 0,
            period = Period};
make_header(PH, Height, Time, Version, Trees, TxsProodHash, Nonce, Difficulty, Period) ->
    AC = case read(PH) of
            {ok, PrevHeader} ->
                pow:sci2int(Difficulty) + 
                     PrevHeader#header.accumulative_difficulty;
            _ -> Height %the parent is unknown
        end,
    #header{prev_hash = PH,
            height = Height,
            time = Time,
            version = Version,
            trees_hash = Trees,
            txs_proof_hash = TxsProodHash,
	    nonce = <<Nonce:256>>,
            difficulty = Difficulty,
            accumulative_difficulty = AC,
            period = Period}.
serialize(H) ->
    false = H#header.prev_hash == undefined,
    HtB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    DB = 16,
    PB = constants:period_bits(),
    HB = constants:hash_size()*8,
    HB = bit_size(H#header.prev_hash),
    HB = bit_size(H#header.trees_hash),
    HB = bit_size(H#header.txs_proof_hash),
    HB = bit_size(H#header.nonce),
    <<(H#header.prev_hash)/binary,
     (H#header.height):HtB,
     (H#header.time):TB,
     (H#header.version):VB,
     (H#header.trees_hash)/binary,
     (H#header.txs_proof_hash)/binary,
     (H#header.difficulty):DB,
     (H#header.nonce)/binary,
     (H#header.period):PB
    >>.
deserialize(H) ->
    HB = constants:hash_size()*8,
    HtB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    PB = constants:period_bits(),
    DB = 16,
    <<PrevHash:HB/bitstring,
     Height:HtB,
     Time:TB,
     Version:VB,
     TreesHash:HB/bitstring,
     TxsProofHash:HB/bitstring,
     Difficulty:DB,
     Nonce:HB/bitstring,
     Period:PB
    >> = H,
    #header{prev_hash = PrevHash,
            height = Height,
            time = Time,
            version = Version,
            trees_hash = TreesHash,
            txs_proof_hash = TxsProofHash,
            difficulty = Difficulty,
            period = Period,
            nonce = Nonce}.
difficulty_should_be(A) ->
    D1 = A#header.difficulty,
    RF = constants:retarget_frequency(),
    Height = A#header.height,
    X = Height rem RF,
    if
        (X == 0) and (not(Height < 10)) ->
            difficulty_should_be2(A);
        true ->
            D1
    end.
difficulty_should_be2(Header) ->
    F = constants:retarget_frequency() div 2,
    {Times1, Hash2000} = retarget(Header, F, []),
    {Times2, _} = retarget(Hash2000, F, []),
    M1 = median(Times1),
    M2 = median(Times2),
    Tbig = M1 - M2,
    T = Tbig div F,%T is the estimated block time over last 2000 blocks.
    NT = pow:recalculate(Hash2000#header.difficulty,
                         Header#header.period,
                         max(1, T)),
    max(NT, constants:initial_difficulty()).
retarget(Header, 1, L) -> {L, Header};
retarget(Header, N, L) ->
    {ok, PH} = read(Header#header.prev_hash),
    T = PH#header.time,
    retarget(PH, N-1, [T|L]).
median(L) ->
    S = length(L),
    F = fun(A, B) -> A > B end,
    Sorted = lists:sort(F, L),
    lists:nth(S div 2, Sorted).
empty_data() ->
    GB = block:genesis_maker(),
    Header0 = block:block_to_header(GB),
    HH = block:hash(Header0),
    block_hashes:add(HH),
    #s{top = Header0, 
       top_with_block = Header0,
       headers = dict:store(HH,Header0,dict:new())}.
header_size() ->
    HB = constants:hash_size()*8,
    HtB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    DB = 16,
    ((HB*4) + HtB + TB + VB + DB).
add_to_top(H, T) ->
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    B = length(T) < FT,
    if
        B -> [H|T];
        true ->
            {T2, _} = lists:split(FT-1, T),%remove last element so we only remember ?FT at a time.
            [H|T2]
    end.

test() ->
    H = hash:doit(<<>>),
    Header = setelement(10, make_header(H, 0, 0, 0, H, H, 0, 0, 0), undefined),
    Header = deserialize(serialize(Header)),
    absorb([Header]),
    H1 = hash:doit(serialize(Header)),
    Header2 = setelement(10, make_header(H1, 0, 0, 0, H, H, 0, 0, 0), undefined),
    absorb([Header2]),
    H1 = block:hash(Header),
    {ok, Header} = read(H1),
    success.

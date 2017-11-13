-module(headers).
-behaviour(gen_server).

%% API
-export([prev_hash/1, height/1, time/1, version/1, trees_hash/1, txs_proof_hash/1, nonce/1, difficulty/1, accumulative_difficulty/1,
         check/0,
         absorb/1, read/1, top/0, dump/0, hard_set_top/1,
         make_header/8,
         serialize/1, deserialize/1,
         difficulty_should_be/1]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([test/0]).

-export_type([header/0, height/0, block_header_hash/0, serialized_header/0]).

-record(header, {height :: height(),
                 prev_hash :: block_header_hash(),
                 trees_hash,
                 txs_proof_hash,
                 time,
                 difficulty,
                 version,
                 nonce,
                 accumulative_difficulty = 0}).
-define(LOC, constants:headers_file()).
-record(s, {headers = dict:new() :: dict:dict(block_header_hash(), header()),
            top = #header{} :: header()}).

-type height() :: non_neg_integer().
-type block_header_hash() :: binary().
-opaque header() :: #header{}.
-type serialized_header() :: binary().

%% API functions

prev_hash(H) -> H#header.prev_hash.
height(H) -> H#header.height.
time(H) -> H#header.time.
version(H) -> H#header.version.
trees_hash(H) -> H#header.trees_hash.
txs_proof_hash(H) -> H#header.txs_proof_hash.
nonce(H) -> H#header.nonce.
difficulty(H) -> H#header.difficulty.
accumulative_difficulty(H) -> H#header.accumulative_difficulty.

-spec absorb([header()]) -> ok.
absorb([]) ->
    ok;
absorb([First|T]) when is_binary(First) ->
    A = deserialize(First),
    absorb([A|T]);
absorb([Header | T]) ->
    true = Header#header.difficulty >= constants:initial_difficulty(),
    Hash = block:hash(Header),
    case read(Hash) of
        {ok, _} ->
            lager:info("Absorb header repeat"),
            ok; %don't store the same header more than once.
        error ->
            true = check_pow(Header),%check that there is enough pow for the difficulty written on the block
            %Header#header.height > 1,
            {true, _} = check_difficulty(Header),%check that the difficulty written on the block is correctly calculated
            ok = gen_server:call(?MODULE, {add, Hash, Header})
            %file:write_file(?LOC, serialize(Header), [append]) %we keep all the good headers we know about in the order we learned about them. This is used for sharing the entire history of headers quickly.
    end,
    absorb(T).

check_pow(Header) ->
    MineDiff = Header#header.difficulty,
    Data = block:hash(Header#header{nonce = <<0:256>>}),
    <<Nonce:256>> = Header#header.nonce,
    Serialized = serialize(Header),
    %Hashed = hash:doit(Serialized, constants:hash_size()),
    io:fwrite("check pow "),
    io:fwrite(packer:pack({pow, Data, MineDiff, Nonce})),
    pow:check_pow({pow, Data, MineDiff, Nonce}, constants:hash_size()).
    %Hashed = testnet_hasher:doit(Serialized),
    %Integer = pow:hash2integer(Hashed),
    %Integer > Header#header.difficulty.

check_difficulty(A) ->
    B = case A#header.height < 2 of
            true ->
                constants:initial_difficulty();
            false ->
                {ok, PHeader} = read(A#header.prev_hash),
                difficulty_should_be(PHeader)
        end,
    {B == A#header.difficulty, B}.

-spec read(block_header_hash()) -> {ok, header()}.
read(Hash) -> gen_server:call(?MODULE, {read, Hash}).
check() -> gen_server:call(?MODULE, {check}).

-spec top() -> header().
top() -> 
    X = gen_server:call(?MODULE, {top}),
    false = element(2, X) == undefined,
    X.

dump() -> gen_server:call(?MODULE, {dump}).

-spec hard_set_top(header()) -> ok.
hard_set_top(Header) ->
    gen_server:call(?MODULE, {hard_set_top, Header}).

make_header(PH, 0, Time, Version, TreesHash, TxsProofHash, Nonce, Difficulty) ->
    #header{prev_hash = PH,
	    height = 0, 
	    time = Time, 
	    version = Version,
	    trees_hash = TreesHash,
	    txs_proof_hash = TxsProofHash,
	    nonce = <<Nonce:256>>,
	    difficulty = Difficulty,
	    accumulative_difficulty = 0};
make_header(PH, Height, Time, Version, Trees, TxsProodHash, Nonce, Difficulty) ->
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
            accumulative_difficulty = AC}.
    
txs_hash(X) ->
    testnet_hasher:doit(X).
-spec serialize(header()) -> serialized_header().
serialize(H) ->
    false = H#header.prev_hash == undefined,
    HtB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    DB = 16,
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
      (H#header.nonce)/binary
    >>.

-spec deserialize(serialized_header()) -> header().
deserialize(H) ->
    HB = constants:hash_size()*8,
    HtB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    DB = 16,
    <<
      PrevHash:HB/bitstring,
      Height:HtB,
      Time:TB,
      Version:VB,
      TreesHash:HB/bitstring,
      TxsProofHash:HB/bitstring,
      Difficulty:DB,
      Nonce:HB/bitstring
    >> = H,
    #header{prev_hash = PrevHash,
            height = Height,
            time = Time,
            version = Version,
            trees_hash = TreesHash,
            txs_proof_hash = TxsProofHash,
            difficulty = Difficulty,
            nonce = Nonce}.

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

check_difficulty2(Header) ->
    F = constants:retarget_frequency() div 2,
    {Times1, Hash2000} = retarget(Header, F, []),
    {Times2, _} = retarget(Hash2000, F, []),
    M1 = median(Times1),
    M2 = median(Times2),
    Tbig = M1 - M2,
    T = Tbig div F,%T is the estimated block time over last 2000 blocks.
    NT = pow:recalculate(Header#header.difficulty,
                         constants:block_time(),
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


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

empty_data() ->
    %GB = block:get_by_height(0),
    GB = block:genesis_maker(),
    Header0 = block:block_to_header(GB),
    HH = block:hash(Header0),
    block_hashes:add(HH),
    #s{top = Header0, headers = dict:store(HH,Header0,dict:new())}.
    
%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    K = if
	    X == "" -> 
                empty_data();
	    true -> X
	end,
    {ok, K}.
init_new_unused([]) ->
    X = file:read_file(?LOC),
    GB = block:get_by_height(0),
    GHeader = block:block_to_header(GB),
    GHash = block:hash(GHeader),
    D1 = dict:store(GHash, GHeader, dict:new()),
    S = #s{headers = D1},
    Ka = case X of
             {ok, Y} -> remember_headers(Y, S);
             _ -> S
         end,
    {ok, Ka}.
remember_headers(<<>>, S) -> S;
remember_headers(B, S) ->
    HeaderSize = header_size(),
    <<H:HeaderSize, B2/binary>> = B,
    Header0 = deserialize(<<H:HeaderSize>>),
    {ok, PrevHeader} = read(Header0#header.prev_hash),
    Header = Header0#header{accumulative_difficulty = 
                           PrevHeader#header.accumulative_difficulty +
                           pow:sci2int(Header0#header.difficulty)},
    io:fwrite(packer:pack(Header)),
    io:fwrite("\n"),
    Hash = block:hash(Header),
    D2 = dict:store(Hash, Header, S#s.headers),
    AD = Header#header.accumulative_difficulty,
    AA = S#s.top,
    AF = AA#header.accumulative_difficulty,
    Top = case AD > AF of
              true -> Header;
              false -> AA
          end,
    S2 = S#s{headers = D2, top = Top},
    remember_headers(B2, S2).
header_size() ->
    HB = constants:hash_size()*8,
    HtB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    DB = 16,
    ((HB*4) + HtB + TB + VB + DB).
    
    
    

handle_call({read, Hash}, _From, State) ->
    {reply, dict:find(Hash, State#s.headers), State};
handle_call({check}, _From, State) ->
    {reply, State, State};
handle_call({dump}, _From, _State) ->
    {reply, ok, empty_data()};
handle_call({top}, _From, State) ->
    {reply, State#s.top, State};
handle_call({hard_set_top, Header}, _From, _State) ->
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


test() ->
    H = testnet_hasher:doit(<<>>),
    Header = setelement(10, make_header(H, 0, 0, 0, H, H, 0, 0), undefined),
    Header = deserialize(serialize(Header)),
    absorb([Header]),
    H1 = testnet_hasher:doit(serialize(Header)),
    Header2 = setelement(10, make_header(H1, 0, 0, 0, H, H, 0, 0), undefined),
    absorb([Header2]),
    H1 = block:hash(Header),
    {ok, Header} = read(H1),
    success.

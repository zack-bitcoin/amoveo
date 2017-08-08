-module(headers).
-behaviour(gen_server).

%% API
-export([prev_hash/1, height/1, time/1, version/1, trees_hash/1, txs_proof_hash/1, nonce/1, difficulty/1, accumulative_difficulty/1,
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

-record(header, {height,
                 prev_hash,
                 trees_hash,
                 txs_proof_hash,
                 time,
                 difficulty,
                 version,
                 nonce,
                 accumulative_difficulty = 0}).
-record(s, {headers = dict:new(),
            top = #header{}}).

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
            Header#header.height > 1,
            {true, _} = check_difficulty(Header),%check that the difficulty written on the block is correctly calculated
            ok = gen_server:call(?MODULE, {add, Hash, Header}),
            file:write_file(constants:headers_file(), serialize(Header), [append]) %we keep all the good headers we know about in the order we learned about them. This is used for sharing the entire history of headers quickly.
    end,
    absorb(T).

check_pow(Header) ->
    Serialized = serialize(Header),
    Hashed = hash:doit(Serialized, constants:hash_size()),
    Integer = pow:hash2integer(Hashed),
    Integer > Header#header.difficulty.

check_difficulty(A) ->
    B = case A#header.height < 2 of
            true ->
                constants:initial_difficulty();
            false ->
                {ok, PHeader} = read(A#header.prev_hash),
                difficulty_should_be(PHeader)
        end,
    {B == A#header.difficulty, B}.

read(Hash) ->
    gen_server:call(?MODULE, {read, Hash}).

top() ->
    gen_server:call(?MODULE, {top}).

dump() ->
    gen_server:call(?MODULE, {dump}).

hard_set_top(Header) ->
    gen_server:call(?MODULE, {hard_set_top, Header}).

make_header(PH, 0, Time, Version, TreesHash, TxsProofHash, Nonce, Difficulty) ->
    #header{prev_hash = PH,
            height = 0,
            time = Time,
            version = Version,
            trees_hash = TreesHash,
            txs_proof_hash = TxsProofHash,
            nonce = Nonce,
            difficulty = Difficulty,
            accumulative_difficulty = 0};
make_header(PH, Height, Time, Version, Trees, TxsProodHash, Nonce, Difficulty) ->
    case read(PH) of
        {ok, PrevHeader} ->
            AC = pow:sci2int(Difficulty) + PrevHeader#header.accumulative_difficulty,
            #header{prev_hash = PH,
                    height = Height,
                    time = Time,
                    version = Version,
                    trees_hash = Trees,
                    txs_proof_hash = TxsProodHash,
                    nonce = Nonce,
                    difficulty = Difficulty,
                    accumulative_difficulty = AC};
        _ -> error %the parent is unknown
    end.

serialize(H) ->
    HB = constants:hash_size()*8,
    HtB = constants:height_bits(),
    TB = constants:time_bits(),
    VB = constants:version_bits(),
    DB = 16,
    HB = bit_size(H#header.prev_hash),
    HB = bit_size(H#header.trees_hash),
    HB = bit_size(H#header.txs_proof_hash),
    <<(H#header.prev_hash)/binary,
      (H#header.height):HtB,
      (H#header.time):TB,
      (H#header.version):VB,
      (H#header.trees_hash)/binary,
      (H#header.txs_proof_hash)/binary,
      (H#header.difficulty):DB,
      (H#header.nonce):HB
    >>.

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
      Nonce:HB
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

median(L) ->
    S = length(L),
    F = fun(A, B) -> A > B end,
    Sorted = lists:sort(F, L),
    lists:nth(S div 2, Sorted).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server callbacks

init([]) ->
    {ok, #s{}}.

handle_call({read, Hash}, _From, State) ->
    {reply, dict:find(Hash, State#s.headers), State};
handle_call({dump}, _From, _State) ->
    {reply, ok, #s{}};
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

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

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

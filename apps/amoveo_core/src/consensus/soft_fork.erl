-module(soft_fork).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        test/0,
        add/3, easy_add/3, check/2, check/3, cfg/0]).

-define(LOC, constants:soft_fork()).

-record(d, {bh2p = dict:new(), 
            m = (mtree:new_empty(32,32,0))
           }).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    K = if
            X == "" -> #d{};
            true -> X
        end,
    {ok, K}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("soft_fork died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, PrevHash, BH, Leaves}, X) -> 
    CFG = mtree:cfg(X#d.m),
    PrevRoot = 
        case dict:find(PrevHash, X#d.bh2p) of
            {ok, Root} -> Root;
            error -> 1
        end,
    {Root2, M2} = mtree:store_batch(Leaves, PrevRoot, X#d.m),
    BH2P2 = dict:store(BH, Root2, X#d.bh2p),
    X2 = X#d{bh2p = BH2P2,
             m = M2},
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call(cfg, _From, X) -> 
    C = mtree:cfg(X#d.m),
    {reply, C, X};
handle_call({check, BH, Txid}, _From, X) -> 
    CFG = mtree:cfg(X#d.m),
    <<N:256>> = Txid,
    Leaf = leaf:new(N, Txid, 0, CFG),
    Path = leaf:path(Leaf, CFG),
    case dict:find(BH, X#d.bh2p) of
        {ok, Root} ->
            Y = case mtree:get(Path, Root, X#d.m) of
                    {_, empty, _} -> false;
                    {_, L, _} -> true
                end,
            {reply, Y, X};
        error ->
            {reply, <<"unknown blockhash">>, X}
    end;
handle_call(_, _From, X) -> {reply, X, X}.

cfg() ->
    gen_server:call(?MODULE, cfg).
check(market_liquidity_tx, BH, Txid) ->
    check(BH, Txid);
check(_, _, _) ->
    false.
check(BH, Txid) ->
    <<_:256>> = BH,
    <<_:256>> = Txid,
    gen_server:call(?MODULE, {check, BH, Txid}).

easy_add(PrevHash, BlockHash, Txs) ->
    Txids = easy_add2(Txs),
    add(PrevHash, BlockHash, Txids).
easy_add2([]) -> [];
easy_add2([{signed, Tx, _, _}|T]) -> 
    case element(1, Tx) of
        market_liquidity_tx ->
            [hash:doit(Tx)];
        _ -> []
    end ++
        easy_add2(T);
easy_add2([_|T]) -> 
    easy_add2(T).


add(_, _, []) -> ok;
add(PrevHash, BH, Txids) ->
    <<_:256>> = BH,
    <<_:256>> = PrevHash,
    lists:map(fun(I) ->
                      <<_:256>> = I
              end, Txids),
    CFG = cfg(),
    Leaves = 
        lists:map(fun(Txid) ->
                          <<N:256>> = Txid,
                          leaf:new(N, Txid, 0, CFG)
                  end, Txids),
    gen_server:cast(?MODULE, {add, PrevHash, BH, Leaves}).


test() ->
    BH = hash:doit(1),
    Txid = hash:doit(2),
    add(hash:doit(0),
      BH,
        [hash:doit(1),
         Txid,
         hash:doit(3),
         hash:doit(4)]),
    [true, false] = 
        [check(BH, Txid),
         check(BH, hash:doit(5))],
        success.
    

-module(tx_pool_feeder).
-behaviour(gen_server).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3, absorb_dump/2]).
-export([absorb/1, absorb/2, absorb_async/1, is_in/2,
	 dump/1]).
-include("../records.hrl").
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
init(ok) -> 
    %process_flag(trap_exit, true),
    {ok, []}.
%TODO using a self() inside of this isn't good, because it is already a gen server listening for messages. and the two kinds of messages are interfering.
handle_call({absorb, SignedTx, Timeout}, _From, State) when (is_integer(Timeout) and (Timeout > -1)) ->
    R = absorb_timeout(SignedTx, Timeout),
    {reply, R, State};
handle_call({absorb, SignedTx}, _From, State) ->
    R = absorb_internal(SignedTx),
    {reply, R, State};
handle_call(_, _, S) -> {reply, S, S}.
handle_cast({dump, Block}, S) -> 
    tx_pool:dump(Block),
    {noreply, S};
handle_cast({absorb, SignedTxs}, S) -> 
    ai2(SignedTxs),
    {noreply, S};
handle_cast({absorb_dump, Block, SignedTxs}, S) -> 
    tx_pool:dump(Block),
    ai2(SignedTxs),
    {noreply, S};
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) ->
    ok.
    %io:fwrite("tx_pool_feeder died\n").
code_change(_, S, _) -> {ok, S}.
is_in(_, []) -> false;
is_in(Tx, [STx2 | T]) ->
    Tx2 = signing:data(STx2),
    (Tx == Tx2) orelse (is_in(Tx, T)).
absorb_internal(SignedTx) ->
    %io:fwrite("tx pool feeder absorb internal\n"),
    Wait = case application:get_env(amoveo_core, kind) of
	       {ok, "production"} -> 2000;
	       %_ -> 400
	       _ -> 2000
	   end,
    absorb_timeout(SignedTx, Wait).
absorb_timeout(SignedTx, Wait) ->
    %io:fwrite("tx pool feeder absorb timeout\n"),
    S = self(),
    H = block:height(),
    Tx = signing:data(SignedTx),
%    F36 = forks:get(36),
%    F38 = forks:get(38),
    Txid = hash:doit(Tx),
    PrevHash = block:hash(headers:top_with_block()),
    spawn(fun() ->
                  %io:fwrite("tx pool feeder absorb timeout b\n"),
                  absorb_internal2(SignedTx, S)
          end),
    receive
        X when (element(1, X) == dict) -> 
            tx_pool:absorb_tx(X, SignedTx),
            ok;
        error -> error;
        Y -> {error, Y}
                 
    after 
        Wait -> timeout_error
%            end
    end.
	    
	    
absorb_internal2(SignedTx, PID) ->
    %io:fwrite("now 2 "),%200
    %io:fwrite("absorb internal 2\n"),
    %io:fwrite(packer:pack(now())),
    %io:fwrite("\n"),
    %io:fwrite("tx pool feeder absorb timeout 2\n"),
    Tx = signing:data(SignedTx),
    F = tx_pool:get(),
    Txs = F#tx_pool.txs,
    %io:fwrite("absorb internal 4"),
    case is_in(Tx, Txs) of
        true -> 
            io:fwrite("is in error\n"),
            PID ! error;
        false -> 
	    true = signing:verify(SignedTx),
	    Fee = element(4, Tx),
	    Type = element(1, Tx),
            %io:fwrite("now 3 "),%1500
            %io:fwrite(packer:pack(now())),
            %io:fwrite("\n"),
	    {ok, MinimumTxFee} = application:get_env(amoveo_core, minimum_tx_fee),
	    B = case Type of
                    multi_tx ->
                        MTxs = Tx#multi_tx.txs,
                        Cost = sum_cost(MTxs, F#tx_pool.dict, F#tx_pool.block_trees),
                                                %io:fwrite("now 4 2"),%500
                                                %io:fwrite(packer:pack(now())),
                    %io:fwrite("\n"),
                        MF = MinimumTxFee * length(MTxs),
                        Fee > (MF + Cost);
                    contract_timeout_tx2 ->
                        Fee > MinimumTxFee;
                    _ ->
                        Cost = governance:value(trees:get(governance, Type, F#tx_pool.dict, F#tx_pool.block_trees)),
                                                %io:fwrite("now 4 "),%500
                                                %io:fwrite(packer:pack(now())),
                    %io:fwrite("\n"),
                        Fee > (MinimumTxFee + Cost)
		    %true
                end,
            if
                not(B) -> 
                    io:fwrite("not enough fees"),
                    PID ! error;
                true -> 
                    %io:fwrite("enough fee \n"),
                    %io:fwrite("now 5 "),%2000
                    %io:fwrite(packer:pack(now())),
                    %io:fwrite("\n"),
            %OldDict = proofs:facts_to_dict(F#tx_pool.facts, dict:new()),
                    %io:fwrite("tx pool feeder absorb timeout 2 2\n"),
                    Height = block:height(),
                    {CBTX, _} = coinbase_tx:make(constants:master_pub(), F#tx_pool.block_trees),
                    Txs2 = [SignedTx|Txs],
                    %io:fwrite("tx pool feeder absorb timeout 2 3\n"),
                    Querys = proofs:txs_to_querys([CBTX|Txs2], F#tx_pool.block_trees, Height+1),
                    %io:fwrite("tx pool feeder absorb timeout 2 4\n"),
                    OldDict = lookup_merkel_proofs(F#tx_pool.dict, Querys, F#tx_pool.block_trees, Height+1),
                    %io:fwrite("tx pool feeder absorb timeout 2 5\n"),
                    MinerReward = block:miner_fees(Txs2),
                    %io:fwrite("tx pool feeder absorb timeout 2 6\n"),
                    GovFees = block:gov_fees(Txs2, OldDict, Height),
                    X = txs:digest([SignedTx], OldDict, Height+1),
                    X2 = txs:digest([CBTX, SignedTx], OldDict, Height+1),
                    %io:fwrite("tx_pool_feeder digested tx.\n"),
                    
                    MinerAccount2 = accounts:dict_update(constants:master_pub(), X2, MinerReward - GovFees, none),
                    NewDict2 = accounts:dict_write(MinerAccount2, X2),
                    %io:fwrite("tx_pool_feeder paid miner.\n"),
                    %Facts = proofs:prove(Querys, F#tx_pool.block_trees),
                    %Facts = trees2:get_proof(Querys, F#tx_pool.block_trees, fast),
                    Facts20 = trees2:get(Querys, F#tx_pool.block_trees),%for the empty thing, instead of storing the key as {accounts, Pub}, it is just a 32 byte hash.
                    Facts2 = lists:map(fun({Key, empty}) -> 
                                               {Key, Key};
                                          ({Key, Value}) -> {Key, Value}
                                       end, Facts20),
                    
                    %io:fwrite("tx_pool_feeder got proofs.\n"),
                    %Dict_old = proofs:facts_to_dict(Facts, dict:new()),
                    Dict = lists:foldl(fun({Key, Value}, D) -> dict:store(Key, Value, D) end, dict:new(), Facts2),
                    %io:fwrite({Dict, Dict2}),
                    %io:fwrite({lists:map(fun(X) -> {X, dict:find(X, Dict)} end, dict:fetch_keys(Dict)), Facts2}),
                    %io:fwrite("tx_pool_feeder facts in a dict.\n"),
                    SameLength = (length(dict:fetch_keys(Dict)) ==
                                      length(dict:fetch_keys(NewDict2))),
                    if
                        SameLength -> ok;
                        true -> io:fwrite({length(dict:fetch_keys(Dict)), 
                                           length(dict:fetch_keys(NewDict2)),
                                           length(dict:fetch_keys(X2)),
                                           length(dict:fetch_keys(OldDict))
                                          })
                    end,
                    NC = block:no_counterfeit(Dict, NewDict2, Txs2, Height+1),
                    %io:fwrite("no counterfeit.\n"),
                    if
                        NC > 0 -> 
                            io:fwrite("counterfeit error \n"),
                            PID ! error;
                        true ->
                            %TODO, only absorb this tx if it was processed in a small enough amount of time.
                            %tx_pool:absorb_tx(X, SignedTx),
                            %io:fwrite("absorb this tx.\n"),
                            PID ! X
                    end
            end
    end.
sum_cost([], _, _) -> 0;
sum_cost([H|T], Dict, Trees) ->
    Type = element(1, H),
    Cost = case Type of
               contract_timeout_tx2 -> 0;
               _ -> governance:value(
                      trees:get(governance, Type, 
                                Dict, Trees))
           end,
    Cost + sum_cost(T, Dict, Trees).
   
%if the thing is already in the dict, then don't do anything. If it isn't in the dict, then get a copy out of the tree for it. 
lookup_merkel_proofs(Dict, [], _, _) -> Dict;
lookup_merkel_proofs(Dict, [{orders, Key}|T], Trees, Height) ->
    Dict2 = 
	case dict:find({orders, Key}, Dict) of
	    error ->
		Oracles = trees:oracles(Trees),
		{_, Oracle, _} = oracles:get(Key#key.id, Oracles),
		Orders = case Oracle of
			     empty -> orders:empty_book();
			     _ -> oracles:orders(Oracle)
			 end,
		%Orders = Oracle#oracle.orders,
		{_, Val, _} = orders:get(Key#key.pub, Orders),
		Val2 = case Val of
			   empty -> 0;
			   X -> orders:serialize(X)
			       %oracles:orders(Oracle)
		       end,
		dict:store({orders, Key}, Val2, Dict);
	    {ok, _} -> Dict
	end,
    lookup_merkel_proofs(Dict2, T, Trees, Height);
lookup_merkel_proofs(Dict, [{oracle_bets, Key}|T], Trees, Height) ->
    Dict2 = 
	case dict:find({oracle_bets, Key}, Dict) of
	    error ->
		Accounts = trees:accounts(Trees),
		{_, Account, _} = accounts:get(Key#key.pub, Accounts),
		Orders = Account#acc.bets,
		{_, Val, _} = oracle_bets:get(Key#key.id, Orders),
		Val2 = case Val of
			   empty -> 0;
			   X -> oracle_bets:serialize(X)
		       end,
		dict:store({oracle_bets, Key}, Val2, Dict);
	    {ok, _} -> Dict
	end,
    lookup_merkel_proofs(Dict2, T, Trees, Height);
lookup_merkel_proofs(Dict, [{TreeID, Key}|T], Trees, Height) ->
    %HashedKey = trees2:hash_key(TreeID, Key),
    Dict2 = 
	%case dict:find({TreeID, Key}, Dict) of
	case csc:read({TreeID, Key}, Dict) of
	%case csc:read(HashedKey, Dict) of
	    error ->
		%Tree = trees:TreeID(Trees),
		%{_, Val, _} = TreeID:get(Key, Tree),
                if
                    (Key == []) ->
                        io:fwrite({Key, TreeID}),
                        1=2;
                    true -> ok
                end,
                %Val = trees:get(TreeID, HashedKey),
                Val = trees:get(TreeID, Key),

                PS = constants:pubkey_size() * 8,
		Val2 = case Val of
			   empty -> 0;
                           {empty, _} -> 0;
                           {<<Head2:PS>>, Many} ->
                               {<<Head2:PS>>, Many};
			   X -> X
		       end,
		%dict:store({TreeID, Key}, Val2, Dict);
                HashedKey = trees2:hash_key(TreeID, Key),
                case Val2 of
                    0 -> csc:add_empty(TreeID,
                           HashedKey,
                           {TreeID, Key}, Dict);
                    {<<_Head:PS>>, _Many} -> 
                        dict:store({unmatched, HashedKey}, Val2, Dict);
                    _ ->
                        csc:add(
                          TreeID, HashedKey, {TreeID, Key}, Val2,
                          Dict)
                end;
	    {ok, _} -> Dict
	end,
    lookup_merkel_proofs(Dict2, T, Trees, Height).

ai2([]) -> ok;
ai2([H|T]) ->
%    case absorb_internal(H) of
%	error -> ok;
%	NewDict ->
%	    dict:find(sample, NewDict),
%	    tx_pool:absorb_tx(NewDict, H)
%    end,
    absorb_internal(H),
    ai2(T).
   
 
absorb([]) -> ok;%if one tx makes the gen_server die, it doesn't ignore the rest of the txs.
absorb([H|T]) -> absorb(H), absorb(T);
absorb(SignedTx) ->
    N = sync_mode:check(),
    case N of
	normal -> 
	    gen_server:call(?MODULE, {absorb, SignedTx});
	_ -> io:fwrite("warning, transactions don't work if you aren't in sync_mode normal"),
	    %1=2,
	    ok
    end.
absorb([], _) -> ok;
absorb([H|T], Timeout) -> 
    absorb(H, Timeout),
    absorb(T, Timeout);
absorb(Tx, Timeout) -> 
    N = sync_mode:check(),
    case N of
	normal -> 
	    gen_server:call(?MODULE, {absorb, Tx, Timeout});
	_ -> io:fwrite("warning, transactions don't work if you aren't in sync_mode normal"),
	    %1=2,
	    ok
    end.
    
absorb_async(SignedTxs) ->
    N = sync_mode:check(),
    case N of
	normal -> 
	    gen_server:cast(?MODULE, {absorb, SignedTxs});
	_ -> %io:fwrite("warning, transactions don't work well if you aren't in sync_mode normal")
	    ok
    end.
absorb_dump(Block, STxs) ->
    N = sync_mode:check(),
    case N of
	normal -> 
	    gen_server:cast(?MODULE, {absorb_dump, Block, STxs});
	_ -> ok
    end.
dump(Block) ->
    gen_server:cast(?MODULE, {dump, Block}).

-module(proofs).
-export([prove/2, test/0, hash/1, facts_to_dict/2, txs_to_querys/3, 
         root/1, tree/1, path/1, value/1, %governance_to_querys/1,
         key/1]).
-define(Header, 1).
-record(proof, {tree, value, root, key, path}).
-record(oracle_bet, {id, true, false, bad}).%true, false, and bad are the 3 types of shares that can be purchased from an oracle%
-include("../../records.hrl").

root(X) -> X#proof.root.
key(X) -> X#proof.key.
value(X) -> X#proof.value.
path(X) -> X#proof.path.
tree(X) -> int_to_tree(X#proof.tree).

%we use a deterministic merge sort that removes repeats while sorting.
tree_to_int(accounts) -> 1;
tree_to_int(channels) -> 2;
tree_to_int(existence) -> 3;
tree_to_int(oracles) -> 5;
tree_to_int(governance) -> 6;
tree_to_int(oracle_bets) -> 7;%
tree_to_int(orders) -> 8;%
tree_to_int(multi_tx) -> 9;
tree_to_int(matched) -> 10;
tree_to_int(unmatched) -> 11;
tree_to_int(sub_accounts) -> 12;
tree_to_int(contracts) -> 13;
tree_to_int(trades) -> 14;
tree_to_int(markets) -> 15;
tree_to_int(receipts) -> 16;
tree_to_int(stablecoins) -> 17.

int_to_tree(1) -> accounts;
int_to_tree(2) -> channels;
int_to_tree(3) -> existence;
int_to_tree(5) -> oracles;
int_to_tree(6) -> governance;
int_to_tree(7) -> oracle_bets;%
int_to_tree(8) -> orders;%
int_to_tree(9) -> multi_tx;
int_to_tree(10) -> matched;
int_to_tree(11) -> unmatched;
int_to_tree(12) -> sub_accounts;
int_to_tree(13) -> contracts;
int_to_tree(14) -> trades;
int_to_tree(15) -> markets;
int_to_tree(16) -> receipts;
int_to_tree(17) -> stablecoins.

leaf_type2tree(empty) -> empty;
leaf_type2tree(accounts) -> accounts;
leaf_type2tree(acc) -> accounts;
leaf_type2tree(oracle) -> oracles;
leaf_type2tree(oracles) -> oracles;
leaf_type2tree(unmatched) -> unmatched;
leaf_type2tree(unmatched_head) -> unmatched;
leaf_type2tree(matched) -> matched;
leaf_type2tree(sub_acc) -> sub_accounts;
leaf_type2tree(sub_accounts) -> sub_accounts;
leaf_type2tree(multi_tx) -> multi_tx;
leaf_type2tree(contracts) -> contracts;
leaf_type2tree(contract) -> contracts;
leaf_type2tree(trades) -> trades;
leaf_type2tree(trade) -> trades;
leaf_type2tree(markets) -> markets;
leaf_type2tree(market) -> markets;
leaf_type2tree(receipts) -> receipts;
leaf_type2tree(stablecoins) -> stablecoins.

    

%deterministic merge-sort    
compare({Ta, Ka}, {Tb, Kb}) ->
    T0 = tree_to_int(Ta),
    T1 = tree_to_int(Tb),
    if
        T0 < T1 -> true;
	T0 > T1 -> false;
	Ka < Kb -> true;
	Ka > Kb -> false;
	true -> repeat
    end.
merge([], []) -> [];
merge([], T) -> T;
merge(T, []) -> T;
merge([A|B], [S|T]) ->
    C = compare(A, S),
    case C of
	repeat -> [A|merge(B, T)];
	true -> [A|merge(B, [S|T])];
	false -> [S|merge([A|B], T)]
    end.
det_improve([]) -> [];
det_improve([A]) -> [A];
det_improve([A|[B|T]]) ->
    [merge(A, B)|det_improve(T)].
det_helper([]) -> [];
det_helper([A]) -> A;
det_helper(L) ->
    det_helper(det_improve(L)).
to_lists([]) -> [];
to_lists([A|T]) -> [[A]|to_lists(T)].
det_order(Querys) ->    
    F = to_lists(Querys),
    det_helper(F).
%finished defining merge-sort.       

remove_repeats([]) -> [];
remove_repeats([X, X|T]) -> 
    remove_repeats([X|T]);
remove_repeats([A|B]) -> 
    [A|remove_repeats(B)].

prove(Querys, Trees) when is_integer(Trees) ->
    X = case application:get_env(amoveo_core, kind) of
            {ok, "production"} -> small;
            _ -> fast
        end,
    trees2:get_proof(Querys, Trees, X);
prove(Querys, Trees) ->
    F2 = det_order(Querys),
    prove2(F2, Trees).
prove2([], _) ->
    [];
prove2([{orders, Key}|T], Trees) ->%
    Oracles = trees:oracles(Trees),%
    {_, Data0, _} = oracles:get(Key#key.id, Oracles),%
    OrdersTree = %
        if%
            Data0 == empty ->%
                orders:empty_book();%
            true ->%
                oracles:orders(Data0)%
        end,%
    {Root, Data, Path} = orders:get(Key#key.pub, OrdersTree),%
    Data2 = case Data of%
		empty -> 0;%
		_ -> orders:serialize(Data)%
	    end,%
    Proof = #proof{root = Root,%
		   key = Key,%
		   path = Path,%
		   value = Data2,%
		   tree = tree_to_int(orders)},%
    true = orders:verify_proof(Root, Key#key.pub, Data2, Path),%
    [Proof|prove2(T, Trees)];%
prove2([{oracle_bets, Key}|T], Trees) ->%
    Accounts = trees:accounts(Trees),%
    {_, Data0, _} = accounts:get(Key#key.pub, Accounts),%
    OrdersTree = Data0#acc.bets,%
    {Root, Data, Path} = oracle_bets:get(Key#key.id, OrdersTree),%
    Data2 = case Data of%
		empty -> 0;%
		_ -> oracle_bets:serialize(Data)%
	    end,%
    Proof = #proof{root = Root,%
		   key = Key,%
		   path = Path,%
		   value = Data2,%
		   tree = tree_to_int(oracle_bets)},%
    true = oracle_bets:verify_proof(Root, Key#key.id, Data2, Path),%
    [Proof|prove2(T, Trees)];%
prove2([{Tree, Key}|T], Trees) ->
    Branch = trees:Tree(Trees),
    {Root, Data, Path} = Tree:get(Key, Branch),
    PS = constants:pubkey_size() * 8,
    Data2 = case Data of
		empty -> 0;
                {<<X:PS>>, Many} -> unmatched:serialize_head(<<X:PS>>, Many);
		_ -> Tree:serialize(Data)
	    end,
    Proof = #proof{root = Root,
		  key = Key,
		  path = Path, 
		  value = Data2,
		  tree = tree_to_int(Tree)},
    true = Tree:verify_proof(Root, Key, Data2, Path),
    [Proof|prove2(T, Trees)].
facts_to_dict([], D) -> D;
facts_to_dict([F|T], D) ->
    Tree = int_to_tree(F#proof.tree),
    Key2 = 
        case Tree of%
            orders -> F#proof.key#key.pub;%
            oracle_bets -> F#proof.key#key.id;%
	    _ -> F#proof.key
	end,
    true = 
        Tree:verify_proof(
          F#proof.root,
          Key2,
          F#proof.value,
          F#proof.path),
    Key = F#proof.key,
    Value0_0 = F#proof.value,
    HK = trees2:hash_key(Tree, Key),
    D2 = case Value0_0 of
        0 ->
            csc:add_empty(Tree, HK, 
                          {Tree, Key}, D);
        _ ->
                 Value0 = 
                     Tree:deserialize(Value0_0),
                 Value3 = 
                     case Tree of
                         accounts -> 
                             Value0#acc{bets = 0};
                         oracles -> 
                             Value0#oracle{
                               orders = 0};
                         _ -> Value0
                     end,
                 csc:add(Tree, HK, {Tree, Key}, 
                         Value3, D)
         end,
    facts_to_dict(T, D2);
facts_to_dict({_VerkleProof, Leaves}, D) ->
    lists:foldl(
      fun(Leaf, Acc) ->
              %Key = trees2:key(Leaf),
              Key = dict_key(Leaf),
              Tree0 = element(1, Leaf),
              if
                  true -> ok;
                  Tree0 == empty -> 
                      io:fwrite({Leaves});
                  true -> ok
              end,
              Tree = leaf_type2tree(Tree0),
              HK = trees2:hash_key(Tree, Key),
              if
                  is_tuple(Leaf) and (size(Leaf) == 2) and (element(1, Leaf) == Tree) ->
                      csc:add_empty(Tree, HK, {Tree, Key}, Acc);
                  true ->
                      csc:add(Tree, HK, {Tree, Key}, Leaf, Acc)
              end
              %Value = trees2:serialize(Leaf),
                  %dict:store({Tree, Key}, Leaf, Acc)
      end, D, Leaves);
facts_to_dict(A, _) ->
    io:fwrite(A),
    ok.

-record(unmatched, {account, %pubkey of the account
		    oracle, %oracle id
		    amount,
		    pointer}).
-record(receipt, {id, tid, pubkey, nonce}).

dict_key({_, K}) -> K;
dict_key(#acc{pubkey = Pub}) -> Pub;
dict_key(#oracle{id = X}) -> X;
dict_key(#matched{account = A, oracle = O}) -> 
    {key, A, O};
dict_key(#unmatched{account = A, oracle = O}) -> 
    {key, A, O};
dict_key({unmatched_head, Pointer, 
          Many, Oracle}) -> 
    {key, <<1:520>>, Oracle};
dict_key(#sub_acc{pubkey = P, contract_id = CID, 
                  type = T}) -> 
    if
        is_integer(CID) -> 1=2;
        true -> ok
    end,
    sub_accounts:make_v_key(P, CID, T);
dict_key(C = #contract{}) -> 
    contracts:make_id(C);
dict_key(#trade{value = V}) -> {key, V};
dict_key(#market{id = X}) -> X;
dict_key(#receipt{id = Z}) -> {key, Z}.

hash(F) ->
    hash:doit(F).
%governance_to_querys(Gov) ->
%    Leaves = trie:get_all(Gov, governance),
%    Keys = leaves_to_querys(Leaves).
%leaves_to_querys([]) -> [];
%leaves_to_querys([L|T]) ->
%    Q = {governance, leaf:key(L)},
%    [Q|leaves_to_querys(T)].
-define(n2i(X), governance:name2number(X)).
txs_to_querys([C|T], Trees, Height) -> 
    F52 = forks:get(52),
    Q = txs_to_querys_old([C|T], Trees, Height),
    if
        Height > F52 -> remove_govs(Q);
        true -> Q
    end.
remove_govs([]) -> [];
remove_govs([{governance, _}|T]) -> 
    remove_govs(T);
remove_govs([H|T]) -> 
    [H|remove_govs(T)].


txs_to_querys_old([C|T], Trees, Height) -> 
    case element(1, C) of
        coinbase ->
            F33 = forks:get(33),
            U = if
                    Height > F33 -> 
                        [{governance, ?n2i(max_block_size)}];
                    true -> []
                end,
            [
             {governance, ?n2i(block_reward)},
             {governance, ?n2i(developer_reward)},
             {accounts, constants:master_pub()},
             %{accounts, trees2:hash_key(accounts, constants:master_pub())},
             {accounts, coinbase_tx:from(C)}
             %{accounts, trees2:hash_key(accounts, coinbase_tx:from(C))}
            ] ++ U ++
                txs_to_querys2(T, Trees, Height);
        signed -> txs_to_querys2([C|T], Trees, Height)
    end.
txs_to_querys2([], _, _) -> [];
txs_to_querys2([STx|T], Trees, Height) ->
    F10 = Height > forks:get(10),
    Tx = signing:data(STx),
    PS = constants:pubkey_size() * 8,
    L = case element(1, Tx) of
	    multi_tx ->
                From = multi_tx:from(Tx),
		txs_to_querys_multi(From, multi_tx:txs(Tx), Trees, Height) ++
		    [ 
		      {accounts, multi_tx:from(Tx)}
		     ];
	    create_acc_tx -> 
                [
                 {governance, ?n2i(create_acc_tx)},
                 {accounts, create_account_tx:pubkey(Tx)},
                 {accounts, create_account_tx:from(Tx)}
                ];
	    spend -> 
                [
                 {governance, ?n2i(spend)},
                 {accounts, spend_tx:from(Tx)},
                 {accounts, spend_tx:to(Tx)}
                ];
	    delete_acc_tx -> 
                [
                 {governance, ?n2i(delete_acc_tx)},
                 {accounts, delete_account_tx:from(Tx)},
                 {accounts, delete_account_tx:to(Tx)}
                  ];
            nc_accept ->
                %io:fwrite(packer:pack(Tx)),
                %io:fwrite("\n"),
                F29 = forks:get(29),
                ID0 = new_channel_tx2:cid(Tx),
                FC = max(F29, forks:get(30)),
                Aid1 = new_channel_tx2:acc1(Tx),
                CID = if
                          (Height > FC) ->
                              new_channel_tx:salted_id(Tx);
                          true -> ID0
                      end,
                [
                 {governance, ?n2i(nc)},
                 {accounts, Aid1},
                 {accounts, new_channel_tx2:acc2(Tx)},
                 {channels, CID}
                ];
            nc ->
                F29 = forks:get(29),
                ID0 = new_channel_tx:cid(Tx),
                FC = max(F29, forks:get(30)),
                Aid1 = new_channel_tx:acc1(Tx),
                CID = if
                          (Height > FC) ->
                              new_channel_tx:salted_id(Tx);
                          true -> ID0
                      end,
                [
                 {governance, ?n2i(nc)},
                 {accounts, Aid1},
                 {accounts, new_channel_tx:acc2(Tx)},
                 {channels, CID}
                 %{channels, new_channel_tx:cid(Tx)}
                ];
	    gc -> 
                [
                 {governance, ?n2i(gc)},
                 {accounts, grow_channel_tx:acc1(Tx)},
                 {accounts, grow_channel_tx:acc2(Tx)},
                 {channels, grow_channel_tx:id(Tx)}
                ];
	    ctc2 -> 
                F23 = forks:get(23),
                true = Height > F23,
                [
                 {governance, ?n2i(ctc)},
                 {accounts, channel_team_close_tx2:aid1(Tx)},
                 {accounts, channel_team_close_tx2:aid2(Tx)},
                 {channels, channel_team_close_tx2:id(Tx)}
                ];
	    ctc -> 
                [
                 {governance, ?n2i(ctc)},
                 {accounts, channel_team_close_tx:aid1(Tx)},
                 {accounts, channel_team_close_tx:aid2(Tx)},
                 {channels, channel_team_close_tx:id(Tx)}
                ];
	    csc -> 
                channel_solo_close:to_prove(Tx, Height) ++
                [
                 {governance, ?n2i(csc)},
                 {governance, ?n2i(time_gas)},
                 {governance, ?n2i(space_gas)},
                 {governance, ?n2i(fun_limit)},
                 {governance, ?n2i(var_limit)},
                 {accounts, channel_solo_close:from(Tx)},
                 {channels, channel_solo_close:id(Tx)}
                ];
	    timeout -> 
                [
                 {governance, ?n2i(timeout)},
                 {accounts, channel_timeout_tx:spk_aid1(Tx)},
                 {accounts, channel_timeout_tx:spk_aid2(Tx)},
                 {channels, channel_timeout_tx:cid(Tx)}
                ];
	    cs -> 
                channel_slash_tx:to_prove(Tx, Height) ++
                [
                 {governance, ?n2i(time_gas)},
                 {governance, ?n2i(space_gas)},
                 {governance, ?n2i(fun_limit)},
                 {governance, ?n2i(var_limit)},
                 {governance, ?n2i(cs)},
                 {accounts, channel_slash_tx:from(Tx)},
                 {channels, channel_slash_tx:id(Tx)}
                ];
	    ex -> 
                [
                 {governance, ?n2i(ex)},
                 {accounts, existence_tx:from(Tx)},
                 {existence, existence_tx:commit(Tx)}
                ];
	    oracle_new -> 
                OID = oracle_new_tx:id(Tx),
                AID = oracle_new_tx:from(Tx),
		N2IOIL = ?n2i(oracle_initial_liquidity),
                G = case oracle_new_tx:governance(Tx) of
                        0 -> 
			    FH5 = forks:get(5),%
			    B = FH5 < Height,%
			    %B = false,%
			    OILK = if%
				       B -> ?n2i(oracle_question_liquidity);
				       true -> N2IOIL%
				   end,%
			    [{governance, OILK}];
                        N -> [{governance, N2IOIL},
			      {governance, N}]
                    end,
                F27 = Height > forks:get(27),
		U = if
			(F27 and F10) -> 
			    PS = constants:pubkey_size() * 8,
			    [{unmatched, {key, <<1:PS>>, OID}}];
			true -> []
		    end,
                [
                 {governance, ?n2i(oracle_new)},
                 {governance, ?n2i(governance_change_limit)},
                 {governance, ?n2i(maximum_question_size)},
                 {governance, ?n2i(minimum_oracle_time)},
                 {accounts, AID},
                 {oracles, OID}
                ] ++ G ++ U;
	    oracle_bet -> 
                OID = oracle_bet_tx:id(Tx),
                Pubkeys = [oracle_bet_tx:from(Tx)|
                           oracle_bet_tx:to_prove(OID, Trees)],
                Pubkeys2 = remove(<<?Header:PS>>, Pubkeys),
                Prove = tagify(accounts, Pubkeys) ++ 
                    make_oracle_bets(Pubkeys2, OID, F10) ++
                    make_orders(Pubkeys, OID, F10),
		U = if%
			F10 -> {unmatched, #key{pub = <<?Header:PS>>, id = OID}};
                        true -> {orders, #key{pub = <<?Header:PS>>, id = OID}}%
		    end,%
                OTG = oracle_type_get(Trees, OID, Height),
		[
		 {governance, ?n2i(oracle_bet)},
		 {governance, ?n2i(minimum_oracle_time)},
		 {governance, OTG},
		 {oracles, OID}] ++ [U] ++ Prove;
	    oracle_close -> 
                AID = oracle_close_tx:from(Tx),
                OID = oracle_close_tx:oracle_id(Tx),
                %Oracles = trees:oracles(Trees),
                %{_, Oracle, _} = oracles:get(OID, Oracles),
                Oracle = trees:get(oracles, OID, dict:new(), Trees),
                Gov = Oracle#oracle.governance,
                G = case Gov of
                        0 -> [];
                        _ -> [{governance, Gov}]
                    end,
                From = oracle_close_tx:from(Tx),
                Pubkeys = [From|
                           oracle_bet_tx:to_prove(OID, Trees)],
                Pubkeys2 = remove(<<?Header:PS>>, Pubkeys),
                Prove = tagify(accounts, Pubkeys) ++ %this should probably be Pubkeys2
                    make_oracle_bets(Pubkeys2, OID, F10) ++
                    make_orders(Pubkeys, OID, F10),
		U = if%
			F10 -> [{unmatched, #key{pub = <<?Header:PS>>, id = OID}},
				{matched, #key{pub = Oracle#oracle.creator, id = OID}}];
			true ->%
			    [{orders, #key{pub = <<?Header:PS>>, id = OID}},%
			     {oracle_bets, #key{pub = Oracle#oracle.creator, id = OID}}]%
		    end,%
                OTG = oracle_type_get(Trees, OID, Height),
		[
                 {governance, ?n2i(minimum_oracle_time)},
                 {governance, ?n2i(maximum_oracle_time)},
                 {governance, ?n2i(oracle_close)},
                 {governance, OTG},
                 {governance, ?n2i(oracle_bet)},
                 {oracles, OID}
                ] ++ U ++ Prove ++ G;
	    unmatched -> 
                OID = oracle_unmatched_tx:oracle_id(Tx),
                From = oracle_unmatched_tx:from(Tx),
		U = if %
			F10 -> [{unmatched, #key{pub = <<?Header:PS>>, id = OID}},
				{unmatched, #key{pub = From, id = OID}}];
			true -> [{orders, #key{pub = <<?Header:PS>>, id = OID}},%
				 {orders, #key{pub = From, id = OID}}]%
		    end,%
		[
                 {governance, ?n2i(unmatched)},
                 {accounts, From},
                 {oracles, OID}
                ] ++ U;
	    oracle_winnings -> 
                OID = oracle_winnings_tx:oracle_id(Tx),
                From = oracle_winnings_tx:from(Tx),
		U = if%
			F10 -> [{matched, #key{pub = From, id = OID}}];
			true ->[{oracle_bets, #key{pub = From, id = OID}}]%
		    end,%
		[{governance, ?n2i(minimum_oracle_time)},
                 {governance, ?n2i(oracle_winnings)},
                 {accounts, From},
                 {oracles, OID}
                ] ++ U;
            contract_use_tx ->
                CID = contract_use_tx:cid(Tx),
                %#contract_use_tx{many = MT, source = S, source_type = ST} = Tx,
                %CID = contracts:make_v_id(CH, MT, S, ST),
                From = contract_use_tx:from(Tx),
                SA = use_contract_sub_accounts(Tx),
                #contract_use_tx{
                                  source = Source,
                                  source_type = SourceType
                                } = Tx,
                U = case Source of
                        <<0:256>> -> [];
                        _ -> 
                            SubAdd = sub_accounts:make_v_key(From, Source, SourceType),
                            [{sub_accounts, SubAdd}]
                    end,
                [{accounts, From},
                 {contracts, CID},
                 {governance, ?n2i(contract_use_tx)}
                 ] ++ SA ++ U;
            contract_new_tx ->
                #contract_new_tx{
              from = From,
              contract_hash = CH,
              source = S,
              source_type = ST,
              many_types = MT} = Tx,
                F52 = forks:get(52),
                
                CID = if
                          Height < F52 ->
                              contracts:make_id(CH, MT,S,ST);
                          true -> contracts:make_v_id(CH, MT, S, ST)
                      end,
                [{accounts, From},
                 {contracts, CID},
                 {governance, ?n2i(contract_new_tx)},
                 {governance, ?n2i(max_contract_flavors)}
                ];
            stablecoin_new_tx ->
                #stablecoin_new_tx{
              from = From,
              id = Salt,
              source = Source,
              source_type = SourceType,
              code_hash = CodeHash,
              margin = Margin,
              expiration = Expiration
             } = Tx,
                Code = <<2, 6, (<<Margin:48>>)/binary, 0, (<<Expiration:32>>)/binary, 2, 32, CodeHash/binary, 113>>,
                CH = hash:doit(Code),
                CID = contracts:make_id(CH, 2, Source, SourceType),
                SID = stablecoin_new_tx:id_maker(From, Salt),
                [{accounts, From},
                 {contracts, CID},
                 {stablecoins, SID},
                 {governance, ?n2i(stablecoin_new_tx)}
                ];
            sub_spend_tx ->
                #sub_spend_tx{
              contract = CID,
              type = N,
              from = From,
              to = To
             } = Tx,
                FKey = sub_accounts:make_v_key(From, CID, N),
                TKey = sub_accounts:make_v_key(To, CID, N),
                [{accounts, From},
                 {sub_accounts, FKey},
                 {sub_accounts, TKey},
                 {governance, ?n2i(sub_spend_tx)}
                ];
            contract_evidence_tx ->
                #contract_evidence_tx{
              from = From,
              prove = Prove,
              contract = Code,
              contract_id = ContractID
             } = Tx,
                [{accounts, From},
                 {contracts, ContractID},
                 {governance, ?n2i(fun_limit)},
                 {governance, ?n2i(var_limit)},
                 {governance, ?n2i(time_gas)},
                 {governance, ?n2i(space_gas)},
                 {governance, ?n2i(max_contract_flavors)},
                 {governance, ?n2i(contract_evidence_tx)}
                ] ++ Prove;
            contract_timeout_tx ->
                #contract_timeout_tx{
              contract_id = CID,
              %proof = Proof,
              from = From
             } = Tx,
                %Contracts = trees:contracts(Trees),
                %{_, Contract, _} = contracts:get(CID, Contracts),
                Contract = trees:get(contracts, CID, dict:new(), Trees),
                #contract{
                           %source = Source,
                           %source_type = SourceType,
                           sink = CID2
                         } = Contract,
                U = case CID2 of
                        <<0:256>> -> [];
                        _ -> [{contract, CID2}]
                    end,
                [{accounts, From},
                 {contracts, CID},
                 {governance, ?n2i(contract_timeout_tx)}
                ] ++ U;
            contract_timeout_tx2 ->
                #contract_timeout_tx2{
              contract_id = CID,
              from = From,
              sink = Sink
             } = Tx,
                U = case Sink of
                        0 -> [];
                        _ -> [{contracts, Sink}]
                    end,
                %Contracts = trees:contracts(Trees),
                %{_, Contract, _} = contracts:get(CID, Contracts),
                Contract = trees:get(contracts, CID, dict:new(), Trees),
                U2 = case Contract of
                         empty -> [];
                         #contract{} ->
                             #contract{
                                    sink = Sink2
                                   } = Contract,
                             [{contracts, Sink2}]
                     end,
                ThingsToProve = [{accounts, From},
                 {contracts, CID}
                ] ++ U ++ U2,
                F52 = forks:get(52),
                if
                    Height < F52 ->
                        ThingsToProve;
                    true ->
                        lists:filter(fun({_, X}) ->
                                             not(X == <<0:256>>)
                                     end, ThingsToProve)
                end;
            contract_winnings_tx ->
                #contract_winnings_tx{
              from = From,
              contract_id = CID,
              sub_account = SA,
              winner = Winner,
              row = Row,
              proof = Proof
             } = Tx,
                %Contracts = trees:contracts(Trees),
                %{_, Contract, _} = contracts:get(CID, Contracts),
                Contract = trees:get(contracts, CID, dict:new(), Trees),
                U5 = case Contract of
                         empty ->
                            F51 = forks:get(51),
                            true = Height > F51,
                            [];
                        #contract{} ->
                             #contract{
                           %result = Result,
                                   source = Source,
                                   sink = Sink,
                                   source_type = SourceType
                                  } = Contract,
                             U4 = case Sink of
                                      <<0:256>> -> [];
                                      _ -> [{contracts, Sink}]
                                  end,
                             U1 = case Source of
                                      <<0:256>> ->
                                          [{accounts, Winner}];
                                      _ ->
                                          SA2 = sub_accounts:make_v_key(Winner, Source, SourceType),
                                          [{sub_accounts, SA2}]
                                  end,
                             U3 = case Proof of
                                      PayoutVector when is_list(PayoutVector) ->
                        %win it as a portion of the source
                                          U1;
                                      _->
                                          sub_accounts_loop(Row, Winner, Sink, 1)
                                  end,
                             U3 ++ U4
                     end,
                [{accounts, From},
                 {accounts, Winner},
                 {contracts, CID},
                 {sub_accounts, SA},%sub_account being deleted
                 {governance, ?n2i(contract_winnings_tx)}
                ] ++ U5;
            contract_simplify_tx ->
                #contract_simplify_tx{
              from = From,
              cid = CID,
              cid2 = CID2,
              cid3 = CID3
             } = Tx,
                %Contracts = trees:contracts(Trees),
                %{_, Contract, _} = contracts:get(CID2, Contracts),
                Contract = trees:get(contracts, CID2, dict:new(), Trees),
                U = case Contract of
                        empty -> 
                            F51 = forks:get(51),
                            true = Height > F51,
                            [];
                        #contract{} ->
                            #contract{
                                   sink = Sink
                                  } = Contract,
                            case Sink of
                                <<0:256>> -> [];
                                _ -> [{contracts, CID3}]
                            end
                    end,
                [{accounts, From},
                 {governance, ?n2i(contract_simplify_tx)},
                 {contracts, CID},
                 {contracts, CID2}
                ] ++ U;
            swap_tx ->
                #swap_tx{
              from = Acc2,
              offer = SOffer,
              fee = Fee
             } = Tx,
                Offer = signing:data(SOffer),
                #swap_offer{
                             acc1 = Acc1,
                             cid1 = CID1,
                             type1 = Type1,
                             cid2 = CID2,
                             type2 = Type2,
                             fee1 = Fee1,
                             salt = Salt
                           } = Offer,
                TradeID = swap_tx:trade_id_maker(Acc1, Salt),
                F1 = case Fee1 of
                         0 -> [];
                         _ -> [{accounts, Acc1}]
                     end,
                F2 = case (Fee - Fee1) of
                         0 -> [];
                         _ -> [{accounts, Acc2}]
                     end,
                U = case CID1 of
                        <<0:256>> -> 
                            [{accounts, Acc1},
                             {accounts, Acc2}];
                        _ ->
                            [{sub_accounts,
                              sub_accounts:make_v_key(Acc1, CID1, Type1)},
                             {sub_accounts,
                              sub_accounts:make_v_key(Acc2, CID1, Type1)}]
                    end,
                U2 = case CID2 of
                         <<0:256>> -> 
                             [{accounts, Acc1},
                              {accounts, Acc2}];
                         _ ->
                             [{sub_accounts,
                               sub_accounts:make_v_key(Acc1, CID2, Type2)},
                              {sub_accounts,
                               sub_accounts:make_v_key(Acc2, CID2, Type2)}]
                     end,
                [{governance, ?n2i(swap_tx)},
                 {trades, {key, TradeID}}] ++
                F1 ++ F2 ++ U ++ U2;
            swap_tx2 ->

                #swap_tx2{
              from = Acc2,
              offer = SOffer,
              fee = Fee
             } = Tx,
                Offer = signing:data(SOffer),
                #swap_offer2{
                              acc1 = Acc1,
                              cid1 = CID1,
                              type1 = Type1,
                              cid2 = CID2,
                              type2 = Type2,
                              salt = Salt,
                              parts = Parts,
                              start_nonce = StartNonce
                           } = Offer,
                TradeID = swap_tx:trade_id_maker(Acc1, Salt),
                F2 = case Fee of
                         0 -> [];
                         _ -> [{accounts, Acc2}]
                     end,
                U = case CID1 of
                        <<0:256>> -> 
                            [{accounts, Acc1},
                             {accounts, Acc2}];
                        _ ->
                            [{sub_accounts,
                              sub_accounts:make_v_key(Acc1, CID1, Type1)},
                             {sub_accounts,
                              sub_accounts:make_v_key(Acc2, CID1, Type1)}]
                    end,
                U2 = case CID2 of
                         <<0:256>> -> 
                             [{accounts, Acc1},
                              {accounts, Acc2}];
                         _ ->
                             [{sub_accounts,
                               sub_accounts:make_v_key(Acc1, CID2, Type2)},
                              {sub_accounts,
                               sub_accounts:make_v_key(Acc2, CID2, Type2)}]
                     end,
                F48 = forks:get(48),
                R = if
                        (Parts > 1) or (Height < F48) -> [];
                        true ->
                            Receipt = receipts:new(TradeID, Acc2, StartNonce),
                            RID = receipts:id(Receipt),
                            [{receipts, {key, RID}}]
                    end,
                [{governance, ?n2i(swap_tx2)},
                 {trades, {key, TradeID}}] ++
                F2 ++ U ++ U2 ++ R;
            market_new_tx ->
                #market_new_tx{
              from = From,
              cid1 = CID1,
              type1 = Type1,
              cid2 = CID2,
              type2 = Type2
             } = Tx,
                MID = markets:make_id(CID1, Type1, CID2, Type2),
                MKey = sub_accounts:make_v_key(From, MID, 0),
                U = case CID1 of
                        <<0:256>> -> [];
                        _ ->
                            SubKey1 = sub_accounts:make_v_key(From, CID1, Type1),
                            [{sub_accounts, SubKey1}]
                    end,
                V = case CID2 of
                        <<0:256>> -> [];
                        _ ->
                            SubKey2 = sub_accounts:make_v_key(From, CID2, Type2),
                            [{sub_accounts, SubKey2}]
                    end,
                [{governance, ?n2i(market_new_tx)},
                 {markets, MID},
                 {sub_accounts, MKey},
                 {accounts, From}]
                    ++ U ++ V;
            market_liquidity_tx ->
                #market_liquidity_tx{
              from = From,
              mid = MID,
              cid1 = CID1,
              type1 = Type1,
              cid2 = CID2,
              type2 = Type2
             } = Tx,
                MKey = sub_accounts:make_v_key(From, MID, 0),
                U = case CID1 of
                        <<0:256>> -> [];
                        _ ->
                            SubKey1 = sub_accounts:make_v_key(From, CID1, Type1),
                            [{sub_accounts, SubKey1}]
                    end,
                V = case CID2 of
                        <<0:256>> -> [];
                        _ ->
                            SubKey2 = sub_accounts:make_v_key(From, CID2, Type2),
                            [{sub_accounts, SubKey2}]
                    end,
                [
                 {markets, MID},
                 {sub_accounts, MKey},
                 {accounts, From},
                 {governance, ?n2i(market_liquidity_tx)}]
                    ++ U ++ V;
            market_swap_tx ->
                #market_swap_tx{
              from = From,
              mid = MID,
              cid1 = CID1,
              type1 = Type1,
              cid2 = CID2,
              type2 = Type2
             } = Tx,
                U = case CID1 of
                        <<0:256>> -> [];
                        _ ->
                            SubKey1 = sub_accounts:make_v_key(From, CID1, Type1),
                            [{sub_accounts, SubKey1}]
                    end,
                V = case CID2 of
                        <<0:256>> -> [];
                        _ ->
                            SubKey2 = sub_accounts:make_v_key(From, CID2, Type2),
                            [{sub_accounts, SubKey2}]
                    end,
                [{accounts, From},
                 {markets, MID},
                 {governance, ?n2i(market_trading_fee)},
                 {governance, ?n2i(market_swap_tx)}]
                    ++ U ++ V;
            trade_cancel_tx ->
                #trade_cancel_tx{
              acc = From, salt = Salt
             } = Tx,
                TID = swap_tx:trade_id_maker(From, Salt),
                [{accounts, From},
                 {governance, ?n2i(trade_cancel_tx)},
                 {trades, {key, TID}}
                ];
	    coinbase_old -> 
                [
                 {governance, ?n2i(block_reward)},
                 {governance, ?n2i(developer_reward)},
                 {accounts, constants:master_pub()},
                 {accounts, coinbase_tx:from(Tx)}
                ]
	end,
    L ++ txs_to_querys2(T, Trees, Height).
		 %{governance, ?n2i(oracle_bet)},
		 %{governance, ?n2i(minimum_oracle_time)},
txs_to_querys_multi(From, Txs0, Trees, Height) ->
    %if there is a new_oracle, and an oracle_bet for that same oracle, then remove the oracle_bet from the list of txs. TODO
    {Queries, Txs} = ttqm2(Txs0, [], []),
    Txs2 = lists:map(
             fun(Tx) -> 
                     Type = element(1, Tx),
                     From1 = element(2, Tx),
                     Bool = (not(From1 == 0)) and ((Type == unmatched) or (Type == oracle_winnings)),
                     Tx2 = if
                               Bool -> Tx;
                               true ->
                                   setelement(2, Tx, From)
                           end,
                     {signed, Tx2, "", ""} end,
             Txs),
    Queries ++ txs_to_querys2(Txs2, Trees, Height).

ttqm2([], Q, T) -> {Q, lists:reverse(T)};
ttqm2([Tx|T], Q, R) when is_record(Tx, oracle_new) -> 
    #oracle_new{
                 id = OID
               } = Tx,
    T2 = remove_oracle_bets(OID, T),
    Q1 = if
             T2 == T -> [];
             true -> [{governance, ?n2i(oracle_bet)}]
         end,
    ttqm2(T2, Q1 ++ Q, [Tx|R]);
ttqm2([H|T], Q, R) -> 
    ttqm2(T, Q, [H|R]).

%-record(oracle_bet, {from, nonce, fee, id, type, amount}).

remove_oracle_bets(_OID, []) -> [];
remove_oracle_bets(OID, [Tx|T]) 
  when is_record(Tx, oracle_bet) -> 
    #oracle_bet{
                 id = OID2
               } = Tx,
    if
        (OID == OID2) ->
            remove_oracle_bets(OID, T);
        true -> [Tx|remove_oracle_bets(OID, T)]
    end;
remove_oracle_bets(OID, [Tx|T]) -> 
    [Tx|remove_oracle_bets(OID, T)].
    
                     
                             



%txs_to_querys_multi([]) -> [];
%txs_to_querys_multi([Tx|T]) ->
%    PS = constants:pubkey_size() * 8,
%    L = case element(1, Tx) of
%	    create_acc_tx -> 
%                [
%                 {governance, ?n2i(create_acc_tx)},
%                 {accounts, create_account_tx:pubkey(Tx)}
%                ];
%	    spend -> 
%                [
%                 {governance, ?n2i(spend)},
%                 {accounts, spend_tx:to(Tx)}
%                ]
%	end,
%    L ++ txs_to_querys_multi(T).

	    
    
remove(_, []) -> [];
remove(X, [X|A]) -> remove(X, A);
remove(X, [Y|A]) -> [Y|remove(X, A)].
tagify(_, []) -> [];
tagify(X, [H|T]) ->
    [{X, H}|tagify(X, T)].
make_oracle_bets([], _, _) -> [];
make_oracle_bets([H|T], OID, F10) ->
    A = if%
	    F10 -> {matched, #key{pub = H, id = OID}};
	    true -> {oracle_bets, #key{pub = H, id = OID}}%
	end,%
    [A|make_oracle_bets(T, OID, F10)].
make_orders([], _, _) -> [];
make_orders([H|T], OID, F10) ->
    A = if%
	    F10 -> {unmatched, #key{pub = H, id = OID}};
	    true -> {orders, #key{pub = H, id = OID}}%
	end,%
     [A|make_orders(T, OID, F10)].
test() ->
    headers:dump(),
    %block:initialize_chain(),
    tx_pool:dump(),
    test_txs:mine_blocks(2),
    timer:sleep(150),
    Question = <<>>,
    %OID = <<2:256>>,
    Fee = 20 + constants:initial_fee(),
    Tx = oracle_new_tx:make_dict(constants:master_pub(), Fee, Question, 1 + block:height(), 0, 0),
    %OID = Tx#oracle_new_tx.id,
    OID = oracle_new_tx:id(Tx),
    %{Tx, _} = oracle_new_tx:make(constants:master_pub(), Fee, Question, 1, OID, 0, 0, Trees0),
    tx_pool_feeder:absorb(keys:sign(Tx)),
    test_txs:mine_blocks(1),
    timer:sleep(200),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Pub2 = <<"BL6uM2W6RVAI341uFO7Ps5mgGp4VKZQsCuLlDkVh5g0O4ZqsDwFEbS9GniFykgDJxYv8bNGJ+/NdrFjKV/gJa6c=">>,
    Pub3 = <<"BIG0bGOtCeH+ik2zxohHNOHyydjzIfi2fhKwFCZ0TFh99y+C8eiwHWwWkFrfGtEL7HcKP+5jdQmRc6wfnG32wlc=">>,
    {Pub55, _} = signing:new_key(),
    PS = constants:pubkey_size() * 8,
    Querys = [{accounts, keys:pubkey()},
	      {accounts, keys:pubkey()},%repeats are ignored
	      {accounts, base64:decode(Pub2)},%empty account
              {accounts, Pub55},
              {accounts, Pub3},
              {accounts, <<297:520>>},
              {accounts, <<744:520>>},
	      {governance, block_reward},
	      {governance, 1},
	      {channels, <<1:256>>},
	      {existence, hash:doit(1)},
	      {oracles, OID},
	      {oracles, <<1:256>>},
	      {unmatched, #key{pub = keys:pubkey(), id = OID}},
              {matched, #key{pub = keys:pubkey(), id = OID}}
	      %{orders, #key{pub = keys:pubkey(), id = OID}},
              %{oracle_bets, #key{pub = keys:pubkey(), id = OID}}
	     ],% ++
        %governance_to_querys(trees:governance(Trees)),
    Facts = prove(Querys, Trees),
    ProofRoot = hash(Facts),
    Dict = facts_to_dict(Facts, dict:new()), %when processing txs, we use this dictionary to look up the state.
    Querys2 = dict:fetch_keys(Dict),
    Facts = prove(Querys2, Trees),
    Dict,
    
    ETxs = "g2wAAAAEaARkAAZzaWduZWRoBmQAAmNhbQAAAEEEhVmGzXqC2hD+5Qy6OXlpK62kiYLi9rwx7CAK96HowS4OOgO+1CphnkV5hxSFj9AuOkIGteOq9O0WI3iWLQ2GOmEBYRRtAAAAQQRHXAXlfMl3JIv7Ni5NmiaAhuff/NsmnCCnWElvuaemWoQ2aCFJzogO/dHY9yrDUsIHaqtS+iD1OW3KuPrpBgoCYjuaygBtAAAAYE1FVUNJUUR5Q0p1Y2h6TlEzUXBkbTk4VjFkWGNxQklEUjVlNDFoRWtlMGRvUkVNd2hBSWdKbjcza3hISzhNUXZDVUttcGEzbzRSWkJYR3FoMXNWV2NZZXNyQ3NRVlo4PWpoBGQABnNpZ25lZGgGZAACY2FtAAAAQQSFWYbNeoLaEP7lDLo5eWkrraSJguL2vDHsIAr3oejBLg46A77UKmGeRXmHFIWP0C46Qga146r07RYjeJYtDYY6YQJhFG0AAABBBFRjuCgudSTRU79SVoCBvWi55+N1QethvQI6LKUCoEPHvIfedkQLxnuD2VJHqoLrULmXyexRWs2sOTwyLsdyL+FiO5rKAG0AAABgTUVVQ0lRRG1naWwvSkxGRVJaN05LUEpZMHZFQ21nZUlsNFdkdU5SbmlzWkw2R25ZVFFJZ1dBOExUazNENEVva3EvWUY4U3d4SnljR1Ixd2RLejlRMWpJUmpyeEFzSDQ9amgEZAAGc2lnbmVkaApkAAJuY20AAABBBIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0NhjptAAAAQQRUY7goLnUk0VO/UlaAgb1ouefjdUHrYb0COiylAqBDx7yH3nZEC8Z7g9lSR6qC61C5l8nsUVrNrDk8Mi7Hci/hYTJhA2IAACcQYgAAJxFhAmEEYQFtAAAAYE1FWUNJUUQ4U1hNeUYxQmRnbWRaRVdHbWFFR3JncXRxTXUvRGZJYmZVMnE1eE94ZUdnSWhBTTU3L21wcmFucDdiVTBSK2RoMS9wZjBOeHViVWJIU256UEFrcFY5b1gwNW0AAABgTUVVQ0lIeTdhenJyYmxIdzdSdEVmRVRMcU5ERTdCUUhmb1Rnd29CVHlZV0JKcHd0QWlFQWxPcnRhY1k1NVFSNUZUVUpoVFltbW5TWldtSGZ4cFUvbmExbjJsSVhJdm89aARkAAZzaWduZWRoCmQAAm5jbQAAAEEER1wF5XzJdySL+zYuTZomgIbn3/zbJpwgp1hJb7mnplqENmghSc6IDv3R2Pcqw1LCB2qrUvog9Tltyrj66QYKAm0AAABBBFRjuCgudSTRU79SVoCBvWi55+N1QethvQI6LKUCoEPHvIfedkQLxnuD2VJHqoLrULmXyexRWs2sOTwyLsdyL+FhMmEBYgAAJxBiAAAnEWECYQRhAm0AAABgTUVRQ0lCZHlWUUhxRlZyQWFGMTVsN0NmajlyckU5THI3RFFUWVJrc3c5d3dMek1nQWlBOGZrMXpIVVgwdlN6b0dVQ05JTGRmRER5Y2lNMnlWVldLb0pnTGNUbUZhdz09bQAAAGBNRVFDSUJvV3pJQU9oUExqTXJjN0tnV3ZFOUxhWmdXdllqYTY0Mk10YzE0S3RFdXNBaUFhRktDTmNhQUFSck9NUVNCUmZMKzdPV054aHduaWdwRUZBc1JaL0c3MmVBPT1q",
    %Txs = binary_to_term(base64:decode(ETxs)),
    {Pub30, Priv30} = signing:new_key(),
    {Pub4, _} = signing:new_key(),
    NewTx = create_account_tx:make_dict(Pub30, 10, 10, keys:pubkey()),
    NewTx2 = create_account_tx:make_dict(Pub4, 10, 10, keys:pubkey()),
    CID = <<7:256>>,
    NewTx3 = new_channel_tx:make_dict(CID, keys:pubkey(), Pub3, 1, 1, 1, 1),
    Txs = [keys:sign(NewTx),
           keys:sign(NewTx2),
           signing:sign_tx(NewTx3, Pub3, Priv30)],
    Q2 = txs_to_querys2(Txs, Trees, 1),
    prove(Q2, Trees),
    success.
oracle_type_get(Trees, OID, Height) 
  when is_integer(Trees) ->    
    %the version for verkle trees.
    ?n2i(oracle_question_liquidity);
oracle_type_get(Trees, OID, Height) ->    
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    Gov = Oracle#oracle.governance,
    NF14 = Height < forks:get(14),
    if 
        NF14 -> ?n2i(oracle_initial_liquidity);
        (Gov == 0) -> ?n2i(oracle_question_liquidity);
        true -> ?n2i(oracle_initial_liquidity)
    end.
   
use_contract_sub_accounts(Tx) ->    
    #contract_use_tx{
                      from = Acc,
                      contract_id = CH,
                      many = Many
                    } = Tx,
    ucsa2(Many, Acc, CH).
    %return a list like [{sub_accounts, X}|...]
ucsa2(0, _, _) -> [];
ucsa2(N, Acc, CID) -> 
    Key = sub_accounts:make_v_key(Acc, CID, N),
    [{sub_accounts, Key}] ++ 
        ucsa2(N-1, Acc, CID).

sub_accounts_loop([], _, _, _) -> [];
sub_accounts_loop([<<0:32>>|T],Winner,CID,N) -> 
    sub_accounts_loop(T,Winner,CID,N+1);
sub_accounts_loop([<<X:32>>|T],Winner,CID,N) -> 
    Key = sub_accounts:make_v_key(Winner, CID, N),
    [{sub_accounts, Key}|
     sub_accounts_loop(T,Winner,CID,N+1)].

    

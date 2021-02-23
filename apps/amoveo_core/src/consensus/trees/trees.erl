-module(trees).
-export([accounts/1,channels/1,existence/1,oracles/1,governance/1,
         matched/1,unmatched/1,sub_accounts/1,contracts/1,trades/1, 
         markets/1,stablecoins/1, receipts/1,

	 update_accounts/2,update_channels/2,update_existence/2,update_oracles/2,update_governance/2, update_matched/2, update_unmatched/2, update_sub_accounts/2, update_contracts/2, update_trades/2, update_markets/2, update_stablecoins/2, update_receipts/2,
	 new/6,
	 new2/7, new3/10, new4/11, new5/13,
         empty_tree/1,
	 root_hash/1, name/1, 
	 hash2int/1, verify_proof/5,
         % root_hash2/2, 
         serialized_roots/1,
	 hash2blocks/1, get/4, get/2,
         all_veo/0, trees1to2/1, trees2to3/1, trees3to4/1, trees4to5/1,
         all/1,
         restore/3]).
-include("../../records.hrl").

trees1to2(Trees) ->
    #trees{
           accounts = A,
           channels = C,
           existence = E,
           oracles = O,
           governance = G
          } = Trees,
    #trees2{
             accounts = A,
             channels = C,
             existence = E,
             oracles = O,
             governance = G,
             matched = empty_tree(matched),
             unmatched = empty_tree(unmatched)
           }.
trees2to3(Trees) ->
    #trees2{
           accounts = A,
           channels = C,
           existence = E,
           oracles = O,
           governance = G,
           matched = M,
           unmatched = U
          } = Trees,
    #trees3{
           accounts = A,
           channels = C,
           existence = E,
           oracles = O,
           governance = G,
           matched = M,
           unmatched = U,
           sub_accounts = trees:empty_tree(sub_accounts),
           contracts = trees:empty_tree(contracts),
           trades = trees:empty_tree(trades)
           }.
trees3to4(Trees) ->
    #trees3{
           accounts = A,
           channels = C,
           existence = E,
           oracles = O,
           governance = G,
           matched = M,
           unmatched = U,
           sub_accounts = SA,
           contracts = CO,
           trades = T
          } = Trees,
    #trees4{
             accounts = A,
             channels = C,
             existence = E,
             oracles = O,
             governance = G,
             matched = M,
             unmatched = U,
             sub_accounts = SA,
             contracts = CO,
             trades = T,
             markets = trees:empty_tree(markets)
           }.
trees4to5(Trees) ->
    #trees4{
           accounts = A,
           channels = C,
           existence = E,
           oracles = O,
           governance = G,
           matched = M,
           unmatched = U,
           sub_accounts = SA,
           contracts = CO,
           trades = T,
           markets = M2
          } = Trees,
    #trees5{
             accounts = A,
             channels = C,
             existence = E,
             oracles = O,
             governance = G,
             matched = M,
             unmatched = U,
             sub_accounts = SA,
             contracts = CO,
             trades = T,
             markets = M2,
             receipts = trees:empty_tree(receipts),
             stablecoins = trees:empty_tree(stablecoins)
           }.

name(<<"accounts">>) -> accounts;
name("accounts") -> accounts;
name(<<"channels">>) -> channels;
name("channels") -> channels;
name(<<"existence">>) -> existence;
name("existence") -> existence;
name(<<"oracles">>) -> oracles;
name("oracles") -> oracles;
name(<<"governance">>) -> governance;
name("governance") -> governance;
name(<<"matched">>) -> matched;
name("matched") -> matched;
name(<<"unmatched">>) -> unmatched;
name("unmatched") -> unmatched;
name(<<"sub_accounts">>) -> sub_accounts;
name("sub_accounts") -> sub_accounts;
name(<<"contracts">>) -> contracts;
name("contracts") -> contracts;
name(<<"trades">>) -> trades;
name("trades") -> trades;
name(<<"markets">>) -> markets;
name("markets") -> markets.
empty_tree(X) -> trie:empty(X).
accounts(X = #trees{}) -> X#trees.accounts;%
accounts(X = #trees2{}) -> X#trees2.accounts;%
accounts(X = #trees3{}) -> X#trees3.accounts;%
accounts(X = #trees4{}) -> X#trees4.accounts;
accounts(X = #trees5{}) -> X#trees5.accounts.
channels(X = #trees{}) -> X#trees.channels;%
channels(X = #trees2{}) -> X#trees2.channels;%
channels(X = #trees3{}) -> X#trees3.channels;%
channels(X = #trees4{}) -> X#trees4.channels;
channels(X = #trees5{}) -> X#trees5.channels.
existence(X = #trees{}) -> X#trees.existence;%
existence(X = #trees2{}) -> X#trees2.existence;%
existence(X = #trees3{}) -> X#trees3.existence;
existence(X = #trees4{}) -> X#trees4.existence;
existence(X = #trees5{}) -> X#trees5.existence.
oracles(X = #trees{}) -> X#trees.oracles;%
oracles(X = #trees2{}) -> X#trees2.oracles;%
oracles(X = #trees3{}) -> X#trees3.oracles;%
oracles(X = #trees4{}) -> X#trees4.oracles;
oracles(X = #trees5{}) -> X#trees5.oracles.
governance(X = #trees{}) -> X#trees.governance;%
governance(X = #trees2{}) -> X#trees2.governance;%
governance(X = #trees3{}) -> X#trees3.governance;%
governance(X = #trees4{}) -> X#trees4.governance;
governance(X = #trees5{}) -> X#trees5.governance.
matched(X = #trees2{}) -> X#trees2.matched;%
matched(X = #trees3{}) -> X#trees3.matched;%
matched(X = #trees4{}) -> X#trees4.matched;
matched(X = #trees5{}) -> X#trees5.matched.
unmatched(X = #trees2{}) -> X#trees2.unmatched;%
unmatched(X = #trees3{}) -> X#trees3.unmatched;%
unmatched(X = #trees4{}) -> X#trees4.unmatched;
unmatched(X = #trees5{}) -> X#trees5.unmatched.
sub_accounts(X = #trees3{}) -> X#trees3.sub_accounts;%
sub_accounts(X = #trees4{}) -> X#trees4.sub_accounts;
sub_accounts(X = #trees5{}) -> X#trees5.sub_accounts.
contracts(X = #trees3{}) -> X#trees3.contracts;
contracts(X = #trees4{}) -> X#trees4.contracts;
contracts(X = #trees5{}) -> X#trees5.contracts.
trades(X = #trees3{}) -> X#trees3.trades;
trades(X = #trees4{}) -> X#trees4.trades;
trades(X = #trees5{}) -> X#trees5.trades.
markets(X = #trees4{}) -> X#trees4.markets;
markets(X = #trees5{}) -> X#trees5.markets.
receipts(X = #trees5{}) -> X#trees5.receipts.
stablecoins(X = #trees5{}) -> X#trees5.stablecoins.
    

new5(A, C, E, O, G, M, U, SA, Contracts, T, Markets,S, R) ->
    #trees5{accounts = A, channels = C,
            existence = E, oracles = O, 
            governance = G, matched = M,
            unmatched = U, sub_accounts = SA,
            contracts = Contracts, trades = T,
            markets = Markets,
            stablecoins = S,
            receipts = R}.
new4(A, C, E, O, G, M, U, SA, Contracts, T, Markets) ->
    #trees4{accounts = A, channels = C,
            existence = E, oracles = O, 
            governance = G, matched = M,
            unmatched = U, sub_accounts = SA,
            contracts = Contracts, trades = T,
            markets = Markets}.
new3(A, C, E, O, G, M, U, SA, Contracts, T) ->
    #trees3{accounts = A, channels = C,
            existence = E, oracles = O, 
            governance = G, matched = M,
            unmatched = U, sub_accounts = SA,
            contracts = Contracts, trades = T}.
new2(A, C, E, O, G, M, U) ->
    #trees2{accounts = A, channels = C,
	   existence = E, oracles = O, 
	   governance = G, matched = M,
	   unmatched = U}.
new(A, C, E, _B, O, G) ->%
    #trees{accounts = A, channels = C,%
	   existence = E, %
	   oracles = O, governance = G}.%
update_governance(X = #trees{}, A) ->%
    X#trees{governance = A};%
update_governance(X = #trees2{}, A) ->
    X#trees2{governance = A};
update_governance(X = #trees3{}, A) ->
    X#trees3{governance = A};
update_governance(X = #trees4{}, A) ->
    X#trees4{governance = A};
update_governance(X = #trees5{}, A) ->
    X#trees5{governance = A}.
update_accounts(X = #trees{}, A) ->%
    X#trees{accounts = A};%
update_accounts(X = #trees2{}, A) ->
    X#trees2{accounts = A};
update_accounts(X = #trees3{}, A) ->
    X#trees3{accounts = A};
update_accounts(X = #trees4{}, A) ->
    X#trees4{accounts = A};
update_accounts(X = #trees5{}, A) ->
    X#trees5{accounts = A}.
update_channels(X = #trees{}, A) ->%
    X#trees{channels = A};%
update_channels(X = #trees2{}, A) ->
    X#trees2{channels = A};
update_channels(X = #trees3{}, A) ->
    X#trees3{channels = A};
update_channels(X = #trees4{}, A) ->
    X#trees4{channels = A};
update_channels(X = #trees5{}, A) ->
    X#trees5{channels = A}.
update_existence(X = #trees{}, E) ->%
    X#trees{existence = E};%
update_existence(X = #trees2{}, E) ->
    X#trees2{existence = E};
update_existence(X = #trees3{}, E) ->
    X#trees3{existence = E};
update_existence(X = #trees4{}, E) ->
    X#trees4{existence = E};
update_existence(X = #trees5{}, E) ->
    X#trees5{existence = E}.
update_oracles(X = #trees{}, A) ->%
    X#trees{oracles = A};%
update_oracles(X = #trees2{}, A) ->
    X#trees2{oracles = A};
update_oracles(X = #trees3{}, A) ->
    X#trees3{oracles = A};
update_oracles(X = #trees4{}, A) ->
    X#trees4{oracles = A};
update_oracles(X = #trees5{}, A) ->
    X#trees5{oracles = A}.
update_matched(X = #trees2{}, M) ->
    X#trees2{matched = M};
update_matched(X = #trees3{}, M) ->
    X#trees3{matched = M};
update_matched(X = #trees4{}, M) ->
    X#trees4{matched = M};
update_matched(X = #trees5{}, M) ->
    X#trees5{matched = M}.
update_unmatched(X = #trees2{}, U) ->
    X#trees2{unmatched = U};
update_unmatched(X = #trees3{}, U) ->
    X#trees3{unmatched = U};
update_unmatched(X = #trees4{}, U) ->
    X#trees4{unmatched = U};
update_unmatched(X = #trees5{}, U) ->
    X#trees5{unmatched = U}.
update_sub_accounts(X = #trees3{}, U) ->
    X#trees3{sub_accounts = U};
update_sub_accounts(X = #trees4{}, U) ->
    X#trees4{sub_accounts = U};
update_sub_accounts(X = #trees5{}, U) ->
    X#trees5{sub_accounts = U}.
update_trades(X = #trees3{}, U) ->
    X#trees3{trades = U};
update_trades(X = #trees4{}, U) ->
    X#trees4{trades = U};
update_trades(X = #trees5{}, U) ->
    X#trees5{trades = U}.
update_contracts(X = #trees3{}, U) ->
    X#trees3{contracts = U};
update_contracts(X = #trees4{}, U) ->
    X#trees4{contracts = U};
update_contracts(X = #trees5{}, U) ->
    X#trees5{contracts = U}.
update_markets(X = #trees4{}, U) ->
    X#trees4{markets = U};
update_markets(X = #trees5{}, U) ->
    X#trees5{markets = U}.
update_stablecoins(X = #trees5{}, U) ->
    X#trees5{stablecoins = U}.
update_receipts(X = #trees5{}, U) ->
    X#trees5{receipts = U}.

rh2(Type, Trees, _Roots) ->
    X = trees:Type(Trees),%M is either trees or trees2
    trie:root_hash(Type, X).

serialized_roots(Trees) -> 
    F = fun(K) -> trie:root_hash(K, trees:K(Trees)) end,
    A = F(accounts),
    C = F(channels),
    E = F(existence),
    O = F(oracles),
    G = F(governance),
    X = <<A/binary,
	 C/binary,
	 E/binary,
	 O/binary,
	 G/binary>>,
    case Trees of
	#trees{} -> X;%
	#trees2{} ->
	    M = F(matched),
	    U = F(unmatched),
	    Z = <<X/binary, M/binary, U/binary>>,
	    Z;
        #trees3{} ->
	    M = F(matched),
	    U = F(unmatched),
            SA = F(sub_accounts),
            Con = F(contracts),
            Trades = F(trades),
            Z = <<X/binary, M/binary, U/binary,
                  SA/binary, Con/binary,
                Trades/binary>>,
            Z;
        #trees4{} ->
	    M = F(matched),
	    U = F(unmatched),
            SA = F(sub_accounts),
            Con = F(contracts),
            Trades = F(trades),
            Markets = F(markets),
            <<X/binary, M/binary, U/binary,
              SA/binary, Con/binary,
              Trades/binary, Markets/binary>>;
        #trees5{} ->
	    M = F(matched),
	    U = F(unmatched),
            SA = F(sub_accounts),
            Con = F(contracts),
            Trades = F(trades),
            Markets = F(markets),
            Receipts = F(receipts),
            Stablecoins = F(stablecoins),
            <<X/binary, M/binary, U/binary,
              SA/binary, Con/binary,
              Trades/binary, Markets/binary,
              Receipts/binary, Stablecoins/binary>>
    end.
root_hash(Trees) ->
    Y = serialized_roots(Trees),
    hash:doit(Y).
hash2blocks([]) -> [];
hash2blocks([H|T]) ->
    B = block:get_by_hash(H),
    case B of
        empty -> hash2blocks(T);
        _ -> [B|hash2blocks(T)]
    end.


hash2int(X) ->
    U = size(X),
    U = constants:hash_size(),
    S = U*8,
    <<A:S>> = X,
    A.
verify_proof(TreeID, RootHash, Key, Value, Proof) ->
    CFG = trie:cfg(TreeID),
    V = case Value of
            0 -> empty;
            X -> X
        end,
    Leaf = TreeID:make_leaf(Key, V, CFG),
    verify:proof(RootHash, Leaf, Proof, CFG).
restore(Root, Fact, Meta) ->
    1=2,
    %This example could be useful if we implement syncing by checkpoint.
    Key = proofs:key(Fact),
    Value = case proofs:value(Fact) of
                0 -> empty;
                X -> X
            end,
    Hash = proofs:root(Fact),
    Path = proofs:path(Fact),
    TreeID = proofs:tree(Fact),
    Hash = TreeID:root_hash(Root),
    Hash = proofs:root(Fact),
    KeyInt = TreeID:key_to_int(Key),
    Leaf = leaf:new(KeyInt, Value, Meta, trie:cfg(TreeID)),
    Out = trie:restore(Leaf, Hash, Path, Root, TreeID),
    {Hash, Leaf2, _} = trie:get(KeyInt, Out, TreeID),
    case Leaf2 of %sanity check
        empty -> 
            Value = empty;
        _ -> Leaf = Leaf2
    end,
    Out.
get(TreeID, Key) ->
    TP = tx_pool:get(),
    Trees = TP#tx_pool.block_trees,
    Dict = TP#tx_pool.dict,
    get(TreeID, Key, Dict, Trees).
get(governance, Key, Dict, Trees) ->
    %first check if the thing we want is stored in the RAM Dict for quick access. If not, load it from the hard drive.
    case governance:dict_get_value(Key, Dict) of
	error -> 
	    Governance = trees:governance(Trees),
	    governance:get_value(Key, Governance);
	empty -> 
	    Governance = trees:governance(Trees),
	    governance:get_value(Key, Governance);
	Y -> Y
    end;
get(TreeID, Key, Dict, Trees) ->
    case TreeID:dict_get(Key, Dict) of
	error -> 
	    Tree = trees:TreeID(Trees),
	    {_, A, _} = TreeID:get(Key, Tree),
	    A;
	empty -> 
	    Tree = trees:TreeID(Trees),
	    {_, A, _} = TreeID:get(Key, Tree),
	    A;
	X -> X
    end.

all_veo_h2(accounts, X) ->
    X#acc.balance;
all_veo_h2(channels, X) ->
    channels:bal1(X) + channels:bal2(X);
all_veo_h2(unmatched, X) ->
    unmatched:amount(X);
all_veo_h2(matched, X) ->
    (matched:true(X) + matched:false(X) + matched:bad(X)) div 2.


    
all_veo_helper(Type) ->
    Accounts = trees:Type((tx_pool:get())#tx_pool.block_trees),
    Leafs = trie:get_all(Accounts, Type),
    A2 = lists:map(fun(A) -> all_veo_h2(Type, Type:deserialize(leaf:value(A))) end, Leafs),
    lists:foldl(fun(X, A) -> X+A end, 0, A2).
    
all_veo() ->
    %matched is being over-estimated currently.
    %we should look up the oracle for every matched trade to know what it is really worth. look at block.erl for an example.
    Rest = [accounts, channels, unmatched, matched],
    X = lists:map(fun(N) -> all_veo_helper(N) end, Rest),
    X2 = lists:foldl(fun(A, B) -> A+B end, 0, X),
    X2 - 180500000000.
all(TreeID) ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Receipts = trees:TreeID(Trees),
    All = trie:get_all(Receipts, TreeID),
    lists:map(
      fun(Leaf) ->
	      TreeID:deserialize(leaf:value(Leaf))
      end, All).
	    
channel_rewards([], Accs, Proofs) -> 
    file:write_file(
      "fork_44_account_proofs.db", 
      term_to_binary(Proofs)),
    Accs;
channel_rewards([{Pub, Bal}|T], Accs, Proofs) -> 
    {_, A, Proof} = accounts:get(Pub, Accs),
    if
        is_record(A, acc) ->
            A2 = A#acc{
                   balance = 
                       A#acc.balance + Bal},
            Accs2 = accounts:write(A2, Accs),
            channel_rewards(
              T, Accs2, 
              [{Pub, A, Proof}|Proofs]);
        true ->
            channel_rewards(
              T, Accs, 
              [{Pub, A, Proof}|Proofs])
    end.
    
    
    

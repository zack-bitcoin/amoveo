-module(trees).
-export([accounts/1,channels/1,existence/1,oracles/1,governance/1,matched/1,unmatched/1,
	 update_accounts/2,update_channels/2,update_existence/2,update_oracles/2,update_governance/2, update_matched/2, update_unmatched/2,
	 new/6,
	 new2/7,
	 root_hash/1, name/1, 
	 hash2int/1, verify_proof/5,
         root_hash2/2, serialized_roots/1,
	 hash2blocks/1, get/4, get/2,
         all_veo/0,
         restore/3]).
-include("../../records.hrl").
-record(trees, {accounts, channels, existence,%
		oracles, governance}).%
%we did a hard fork to move the matched and unmatched trees from inside of accounts and oracles to their own tries.
-record(trees2, {accounts, channels, existence,
		oracles, governance, matched,
		unmatched}).
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
name("unmatched") -> unmatched.
accounts(X = #trees{}) -> X#trees.accounts;%
accounts(X) -> X#trees2.accounts.
channels(X = #trees{}) -> X#trees.channels;%
channels(X) -> X#trees2.channels.
existence(X = #trees{}) -> X#trees.existence;%
existence(X) -> X#trees2.existence.
oracles(X = #trees{}) -> X#trees.oracles;%
oracles(X) -> X#trees2.oracles.
governance(X = #trees{}) -> X#trees.governance;%
governance(X) -> X#trees2.governance.
matched(X) -> X#trees2.matched.
unmatched(X) -> X#trees2.unmatched.
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
    X#trees2{governance = A}.
update_accounts(X = #trees{}, A) ->%
    X#trees{accounts = A};%
update_accounts(X = #trees2{}, A) ->
    X#trees2{accounts = A}.
update_channels(X = #trees{}, A) ->%
    X#trees{channels = A};%
update_channels(X = #trees2{}, A) ->
    X#trees2{channels = A}.
update_existence(X = #trees{}, E) ->%
    X#trees{existence = E};%
update_existence(X = #trees2{}, E) ->
    X#trees2{existence = E}.
update_oracles(X = #trees{}, A) ->%
    X#trees{oracles = A};%
update_oracles(X, A) ->
    X#trees2{oracles = A}.
update_matched(X, M) ->
    X#trees2{matched = M}.
update_unmatched(X, U) ->
    X#trees2{unmatched = U}.
root_hash2(Trees, Roots) ->
    A = rh2(accounts, Trees, Roots),
    C = rh2(channels, Trees, Roots),
    E = rh2(existence, Trees, Roots),
    O = rh2(oracles, Trees, Roots),
    G = rh2(governance, Trees, Roots),
    HS = constants:hash_size(),
    HS = size(A),
    HS = size(C),
    HS = size(E),
    HS = size(O),
    HS = size(G),
    X = <<A/binary,
	 C/binary,
	 E/binary,
	 O/binary,
	 G/binary>>,
    Y = case Trees of
	    #trees{} -> X;%
	    #trees2{} ->
		M = rh2(matched, Trees, Roots),
		U = rh2(unmatched, Trees, Roots),
		HS = size(M),
		HS = size(U),
		Z = <<X/binary, M/binary, U/binary>>,
		Z
	end,
    hash:doit(Y).
		
rh2(Type, Trees, Roots) ->
    %M = element(1, Trees),
    X = trees:Type(Trees),%M is either trees or trees2
    Out = case X of
              empty -> 
                  Fun = list_to_atom(atom_to_list(Type) ++ "_root"),
                  block:Fun(Roots);
              Y -> 
                  trie:root_hash(Type, Y)
          end,
    Out.
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
	    Z
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
	empty -> 
	    Governance = trees:governance(Trees),
	    governance:get_value(Key, Governance);
	Y -> Y
    end;
get(TreeID, Key, Dict, Trees) ->
    case TreeID:dict_get(Key, Dict) of
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
	    
    

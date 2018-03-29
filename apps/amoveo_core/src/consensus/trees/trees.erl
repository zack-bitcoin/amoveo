-module(trees).
-export([accounts/1,channels/1,existence/1,
	 oracles/1,new/6,update_accounts/2,
	 update_channels/2,update_existence/2,
	 update_oracles/2,
	 update_governance/2, governance/1,
	 root_hash/1, name/1, 
         %prune/0, prune/1,
	 %prune/2,
	 hash2int/1, verify_proof/5,
         root_hash2/2, serialized_roots/1,
	 hash2blocks/1, dict_tree_get/4,
	 dict_tree_get/2,
         restore/3]).
-include("../../records.hrl").
-record(trees, {accounts, channels, existence,
		oracles, governance}).
name(<<"accounts">>) -> accounts;
name("accounts") -> accounts;
name(<<"channels">>) -> channels;
name("channels") -> channels;
name(<<"existence">>) -> existence;
name("existence") -> existence;
name(<<"oracles">>) -> oracles;
name("oracles") -> oracles;
name(<<"governance">>) -> governance;
name("governance") -> governance.
accounts(X) -> X#trees.accounts.
channels(X) -> X#trees.channels.
existence(X) -> X#trees.existence.
oracles(X) -> X#trees.oracles.
governance(X) -> X#trees.governance.
new(A, C, E, B, O, G) ->
    #trees{accounts = A, channels = C,
	   existence = E, 
	   oracles = O, governance = G}.
update_governance(X, A) ->
    X#trees{governance = A}.
update_accounts(X, A) ->
    X#trees{accounts = A}.
update_channels(X, A) ->
    X#trees{channels = A}.
update_existence(X, E) ->
    X#trees{existence = E}.
update_oracles(X, A) ->
    X#trees{oracles = A}.
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
    hash:doit(<<
               A/binary,
               C/binary,
               E/binary,
               O/binary,
               G/binary
               >>).
rh2(Type, Trees, Roots) ->
    X = trees:Type(Trees),
    Out = case X of
              empty -> 
                  Fun = list_to_atom(atom_to_list(Type) ++ "_root"),
                  block:Fun(Roots);
              Y -> 
                  trie:root_hash(Type, Y)
          end,
    Out.
serialized_roots(Trees) -> 
    A = trie:root_hash(accounts, trees:accounts(Trees)),
    C = trie:root_hash(channels, trees:channels(Trees)),
    E = trie:root_hash(existence, trees:existence(Trees)),
    O = trie:root_hash(oracles, trees:oracles(Trees)),
    G = trie:root_hash(governance, trees:governance(Trees)),
    <<
     A/binary,
     C/binary,
     E/binary,
     O/binary,
     G/binary
     >>.
root_hash(Trees) ->
    hash:doit(serialized_roots(Trees)).
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
dict_tree_get(TreeID, Key) ->
    TP = tx_pool:get(),
    Trees = TP#tx_pool.block_trees,
    Dict = TP#tx_pool.dict,
    dict_tree_get(TreeID, Key, Dict, Trees).
dict_tree_get(governance, Key, Dict, Trees) ->
    %first check if the thing we want is stored in the RAM Dict for quick access. If not, load it from the hard drive.
    case governance:dict_get_value(Key, Dict) of
	empty -> 
	    Governance = trees:governance(Trees),
	    governance:get_value(Key, Governance);
	Y -> Y
    end;
dict_tree_get(TreeID, Key, Dict, Trees) ->
    case TreeID:dict_get(Key, Dict) of
	empty -> 
	    Tree = trees:TreeID(Trees),
	    {_, A, _} = TreeID:get(Key, Tree),
	    A;
	X -> X
    end.

	    
    

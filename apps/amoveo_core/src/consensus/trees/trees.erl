-module(trees).
-export([accounts/1,channels/1,existence/1,oracles/1,governance/1,matched/1,unmatched/1,sortition/1,candidates/1,rng_challenge/1,rng_result/1,sortition_blocks/1,
	 update_accounts/2,update_channels/2,update_existence/2,update_oracles/2,update_governance/2, update_matched/2, update_unmatched/2,update_sortition/2,update_candidates/2,update_rng_challenge/2,update_rng_result/2,update_sortition_blocks/2,
	 new/6, new2/7, new3/12,
         empty_tree/1,
	 root_hash/1, name/1, 
	 hash2int/1, verify_proof/5,
         root_hash2/2, serialized_roots/1,
	 hash2blocks/1, get/4, get/2,
         all_veo/0]).
-include("../../records.hrl").
-record(trees, {accounts, channels, existence,%
		oracles, governance}).%
%we did a hard fork to move the matched and unmatched trees from inside of accounts and oracles to their own tries.
-record(trees2, {accounts, channels, existence,
		oracles, governance, matched,
		unmatched}).
-record(trees3, {accounts, channels, existence,
		oracles, governance, matched,
		unmatched, sortition, candidates,
                rng_challenge, rng_result,
                sortition_blocks}).
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
name(<<"sortition">>) -> sortition;
name("sortition") -> sortition;
name(<<"candidates">>) -> candidates;
name("candidates") -> candidates;
name(<<"matched">>) -> matched;
name("matched") -> matched;
name(<<"unmatched">>) -> unmatched;
name("unmatched") -> unmatched;
name(<<"rng_challenge">>) -> rng_challenge;
name("rng_challenge") -> rng_challenge;
name(<<"rng_result">>) -> rng_result;
name("rng_result") -> rng_result;
name(<<"sortition_blocks">>) -> sortition_blocks;
name("sortition_blocks") -> sortition_blocks.
empty_tree(X) -> trie:empty(X).
accounts(X = #trees{}) -> X#trees.accounts;%
accounts(X = #trees2{}) -> X#trees2.accounts;%
accounts(X) -> X#trees3.accounts.
channels(X = #trees{}) -> X#trees.channels;%
channels(X = #trees2{}) -> X#trees2.channels;%
channels(X) -> X#trees3.channels.
existence(X = #trees{}) -> X#trees.existence;%
existence(X = #trees2{}) -> X#trees2.existence;%
existence(X) -> X#trees3.existence.
oracles(X = #trees{}) -> X#trees.oracles;%
oracles(X = #trees2{}) -> X#trees2.oracles;%
oracles(X) -> X#trees3.oracles.
governance(X = #trees{}) -> X#trees.governance;%
governance(X = #trees2{}) -> X#trees2.governance;%
governance(X) -> X#trees3.governance.
matched(X = #trees2{}) -> X#trees2.matched;
matched(X) -> X#trees3.matched.
unmatched(X = #trees2{}) -> X#trees2.unmatched;
unmatched(X) -> X#trees3.unmatched.
sortition(X) -> X#trees3.sortition.
candidates(X) -> X#trees3.candidates.
rng_challenge(X) -> X#trees3.rng_challenge.
rng_result(X) -> X#trees3.rng_result.
sortition_blocks(X) -> X#trees3.sortition_blocks.
    
new3(A, C, E, O, G, M, U, S, Ca, RC, RR, SB) ->
    #trees3{accounts = A, channels = C,
	   existence = E, oracles = O, 
	   governance = G, matched = M,
	   unmatched = U, sortition = S,
           candidates = Ca, rng_challenge = RC,
           rng_result = RR, sortition_blocks = SB}.
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
    X#trees3{governance = A}.
update_accounts(X = #trees{}, A) ->%
    X#trees{accounts = A};%
update_accounts(X = #trees2{}, A) ->
    X#trees2{accounts = A};
update_accounts(X = #trees3{}, A) ->
    X#trees3{accounts = A}.
update_channels(X = #trees{}, A) ->%
    X#trees{channels = A};%
update_channels(X = #trees2{}, A) ->
    X#trees2{channels = A};
update_channels(X = #trees3{}, A) ->
    X#trees3{channels = A}.
update_existence(X = #trees{}, E) ->%
    X#trees{existence = E};%
update_existence(X = #trees2{}, E) ->
    X#trees2{existence = E};
update_existence(X = #trees3{}, E) ->
    X#trees3{existence = E}.
update_oracles(X = #trees{}, A) ->%
    X#trees{oracles = A};%
update_oracles(X = #trees2{}, A) ->
    X#trees2{oracles = A};
update_oracles(X = #trees3{}, A) ->
    X#trees3{oracles = A}.
update_matched(X = #trees2{}, M) ->
    X#trees2{matched = M};
update_matched(X = #trees3{}, M) ->
    X#trees3{matched = M}.
update_unmatched(X = #trees2{}, U) ->
    X#trees2{unmatched = U};
update_unmatched(X = #trees3{}, U) ->
    X#trees3{unmatched = U}.
update_sortition(X = #trees3{}, U) ->
    X#trees3{sortition = U}.
update_candidates(X = #trees3{}, U) ->
    X#trees3{candidates = U}.
update_rng_challenge(X = #trees3{}, U) ->
    X#trees3{rng_challenge = U}.
update_rng_result(X = #trees3{}, U) ->
    X#trees3{rng_result = U}.
update_sortition_blocks(X = #trees3{}, U) ->
    X#trees3{sortition_blocks = U}.
root_hash2(Trees, _Roots) ->
    A = rh2(accounts, Trees),
    C = rh2(channels, Trees),
    E = rh2(existence, Trees),
    O = rh2(oracles, Trees),
    G = rh2(governance, Trees),
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
		M = rh2(matched, Trees),
		U = rh2(unmatched, Trees),
		HS = size(M),
		HS = size(U),
		Z = <<X/binary, M/binary, U/binary>>,
		Z;
            #trees3{} ->
		M = rh2(matched, Trees),
		U = rh2(unmatched, Trees),
                S = rh2(sortition, Trees),
                Ca = rh2(candidates, Trees),
                RC = rh2(rng_challenge, Trees),
                RR = rh2(rng_result, Trees),
                SB = rh2(sortition_blocks, Trees),
		HS = size(M),
		HS = size(U),
		HS = size(S),
		HS = size(Ca),
		HS = size(RC),
		HS = size(RR),
                HS = size(SB),
		Z = <<X/binary, M/binary, U/binary, S/binary, Ca/binary, RC/binary, RR/binary, SB/binary>>,
                Z
                    
	end,
    hash:doit(Y).
		
rh2(Type, Trees) ->
    X = trees:Type(Trees),
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
            S = F(sortition),
            Ca = F(candidates),
            RC = F(rng_challenge),
            RR = F(rng_result),
            SB = F(sortition_blocks),
	    Z = <<X/binary, M/binary, U/binary, S/binary, Ca/binary, RC/binary, RR/binary, SB/binary>>,
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
	    
    

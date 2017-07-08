-module(trees).
-export([accounts/1,channels/1,existence/1,burn/1,
	 oracles/1,new/6,update_accounts/2,
	 update_channels/2,update_existence/2,
	 update_burn/2,update_oracles/2,
	 update_governance/2, governance/1,
	 root_hash/1, name/1, garbage/0,
	 hash2int/1]).
-record(trees, {accounts, channels, existence,
		burn, oracles, governance}).
name(<<"accounts">>) -> accounts;
name("channels") -> channels;
name("existence") -> existence;
name("burn") -> burn;
name("oracles") -> oracles;
name(<<"governance">>) -> governance.
accounts(X) -> X#trees.accounts.
channels(X) -> X#trees.channels.
existence(X) -> X#trees.existence.
burn(X) -> X#trees.burn.
oracles(X) -> X#trees.oracles.
governance(X) -> X#trees.governance.
new(A, C, E, B, O, G) ->
    #trees{accounts = A, channels = C,
	   existence = E, burn = B, 
	   oracles = O, governance = G}.
update_governance(X, A) ->
    X#trees{governance = A}.
update_accounts(X, A) ->
    X#trees{accounts = A}.
update_channels(X, A) ->
    X#trees{channels = A}.
update_existence(X, E) ->
    X#trees{existence = E}.
update_burn(X, E) ->
    X#trees{burn = E}.
update_oracles(X, A) ->
    X#trees{oracles = A}.
root_hash(Trees) ->
    A = accounts:root_hash(trees:accounts(Trees)),
    C = channels:root_hash(trees:channels(Trees)),
    E = existence:root_hash(trees:existence(Trees)),
    B = burn:root_hash(trees:burn(Trees)),
    O = oracles:root_hash(trees:oracles(Trees)),
    testnet_hasher:doit(<<
			  A/binary,
			  C/binary,
			  E/binary,
			  B/binary,
			  O/binary
			>>).
keepers(_, _, 0) -> [];
keepers(TreeID, Hash, Many) ->
    %io:fwrite(packer:pack({keepers_hash, Hash})),
    BP = block:read(Hash),
    Trees = block:trees(BP),
    Height = block:height(BP),
    Root = trees:TreeID(Trees),
    T = case Height of
	0 -> [];
	N ->
	    keepers(TreeID, block:prev_hash(BP), Many-1)
    end,
    [Root|T].
garbage(TreeID) ->
    Top = top:doit(),
    {ok, RD} = application:get_env(ae_core, revert_depth),
    Keepers = keepers(TreeID, Top, RD),
    trie:garbage(Keepers, TreeID).
    
garbage() ->
    garbage(oracles),
    garbage(channels),
    garbage(accounts),
    garbage(existence),
    garbage(governance).

hash2int(X) ->
    U = size(X),
    U = constants:hash_size(),
    S = constants:hash_size()*8,
    <<A:S>> = X,
    A.
    
    %we also need to garbage orders, oracle_bets, shares, proof of burn.
    %proof of burn doesn't exist yet.
    %The other three are not stored in Trees, they are inside of oracles and accounts, so garbage/1 does not work for them.

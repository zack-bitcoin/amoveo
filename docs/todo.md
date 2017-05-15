in channel_feeder:they_simplify, we need to update all the arbitragable channels.

api needs to be encrypted, especially the stuff about channels.


in channel_solo_close:check_slash, we run next_ss.
This seems like it could be far simpler.


We need to think about channel nonces very carefully.
We don't want old data to be unexepctedly reusable.


in spk prove_facts2, the burn and existence trees store by hash not by integer, so the code needs to be modified for them.


we need tests for:
channel smart contract,
channel smart contract that proves a outcome from an oracle.
channel lightning payment (Also need code for this. Maybe take a look at flying fox or pinkfairy for examples)%%%%
test that the channel pays out the correct amount in every way it could be closed. It looks like channels:update could have some bugs.

merkel should be updated. The tuples of binaries should start with an atom. This way proofs can be encoded as javascript objects.
Alternatively, we could use raw jiffy to encode the proofs.


download blocks talk/1 seems useless. talker:talk is accomplishing the same goal.

It is possible to use channel_slash to store data in a channel such that the channel can't be closed with a channel_timeout.
Maybe this is a mistake.

javascript light wallets need to be able to do all the channel stuff that full nodes do.

It is currently possible for an attacker to trick us into ignoring a good block. They trick us into storing a good blocks hash into block_hashes. They give us a good block's header, but mix in some bad transactions, or censor a transaction, or they don't give us some of the merkel tree we need to verify the transactions.

constants:difficulty_bits() might be too big.


We need to also add a way for the two parties to work together to close the channel early, so they don't have to wait to do a timeout_tx. We can either make a new tx, or make channel_team_close more complicated.


We should use a CLI program to talk to the node instead of using erlang directly.
It should be able to access everything in /src/networking/internal_handler.erl
We should add everything from easy to internal_handler.erl

We need to update download_blocks so that peers get ranked, and we spend more time talking to higher-ranked peers.

There is a problem where if you crash while syncing with a peer, then you skip trying to sync with any peer lower on the list. this is very bad.

make the api networking/handler be entirely encrypted. This is to protect information about the channels. https://github.com/BumblebeeBat/pink_crypto/blob/master/src/encryption.erl

download_blocks could be more efficient.

maybe nodes need to advertise their own IP/port combo as a peer?

It would be nice if there were some macros for chalang/src/compiler_lisp2.erl that did backtracking. that way we wouldn't have to think about control flow when making smart contracts.


Updates for next time we restart at a genesis block:


each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible. This stops transactions from being reused on multiple forks.


Make sure that if something was garbage collected from a merkel tree, and we try accessing the thing, it gives a different message than trying to access something that doesn't exist. Make sure we don't assume a block is invalid just because we don't have the proof of it's validity.


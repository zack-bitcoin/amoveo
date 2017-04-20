javascript light wallets need to be able to do all the channel stuff that full nodes do.


right now there is only 1 type of currency stored on-chain, AE tokens.
We want to store several thousand types.
Each account will contain the merkle root of a trie containing all their tokens of the different types.
Each type of token can be positive, or negative.
If you own negative of type N, and you get positive of type N, they turn into normal AE tokens.
The tokens have a half life. They are each either disappearing, or converting into AE tokens depending on the difficulty.
If the difficulty is above e^(A*N) then the positive are converting to AE and the negative are disappearing.
If the difficulty is below e^(A*N) then the positive are disappearing and the negative are converting to AE.
A is a constant to set the step size between types.

So the types of tokens are all bets about whether the difficulty will on average above or below a number.

Channels should only hold AE tokens, but when a channel settles, it should be able to split the AE tokens into equal pairs of any of the types of tokens, and give one side of the pair to one account and the other side to the other account. This way we can use channels to trade all types of tokens.

spend and create_account transactions should have a hashlock, so we can connect them to events on other blockchains.

we should make sure block hashing, and other hashes are all formatted so it is easy to make a javascript wallet.


in the white paper we should explain the centralized and trustless exchanges.


blog post about channels that use untrusted third parties to be secure. So you don't have to stay online all the time.

channel_slash needs a commit-reveal. That way the miner wont just steal your evidence.



Right now the block creator is incentivized to censor any channel_slash tx and replace it with their own.
We need a way for the slasher to get rewarded for slashing. We need to split it up into a commit-reveal. So you can only slash something that you committed to slashing recently.
   The commit transaction can be combined with out planned proof of existence transaction type.

Multiple users might commit to revealing stuff at the same time. We need to remember all these commitments.
We need a merkle trie to store these commits.

the account channel:slasher() needs to be rewarded in channel_timeout:doit().

in channel_solo_close we update the channel:amount written on-chain. this is a problem, because when we slash, we want to undo the channel_solo_close.
We need to look at the tests, maybe we are testing the wrong thing, or we aren't testing the right thing.

right now in channel_solo_close we have a big check to make sure that we can afford to pay the slash_reward, which is hard-coded into the spk.
instead, we should let them use the turing complete state channels to program stuff like this.
I want to think about this more before I delete the code.
For the same reason we should get rid of channel_rent. This is something that could be programmed.
the big case statement at line 48 of channel_solo_close seems bad. shouldn't we make sure that both balances are positive, not just one?

It is currently possible for an attacker to trick us into ignoring a good block. They trick us into storing a good blocks hash into block_hashes. They give us a good block's header, but mix in some bad transactions, or censor a transaction.
To fix this, each header should contain a hash of all the transactions. We should include the signatures in this hash. This gives us a guarantee that all the data is available at block:check1.

We need a way to close a channel when your partner refuses to sign any SPK. It can either be a new tx type, or we can make solo_close more complex.


constants:difficulty_bits() might be too big.


the new way of doing channel slash has a different problem.
We need the channel to finish at the highest nonce possible. The third party could be bribed to choose a different final state.
We need some way to do the channel_slash transaction again and again, until a higher nonce cannot be found.
Each slasher puts up a deposit, and takes the deposit of the previous.
If the same channel_slash exists for a long enough time period, then anyone can do a channel_timeout transaction, to close the channel.
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

proof of existence transaction type.

each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.


We need to reward the miner with the transaction fees, to incentivize him to include them. block:absorb_txs



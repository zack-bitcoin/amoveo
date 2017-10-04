

verifying a block should not require looking at the previous block. This way we can verify them in parallel.

review how governance locks are working. They are supposed to prevent multiple oracles updating the same governance variable simultaniously.

consider reducing the block time to 2 minutes.
Then we could have faster trading in the markets.

The proofs are currently not deterministic? They contain pointers that only make sense on the node that created the proof?
Making proofs deterministic is an advantage because full nodes don't have to download the proofs. They can generate it themselves, and check that the hash matches.

There is danger that we could be tricked into receiving invalid blocks repeatedly. The severity of this attack is currently in proportion to the size of the block.
So, we should merkelize the downloading of the blocks. The proofs and txs should be in merkle trees.
This way we can verify each piece as we get it and reject bad pieces. If one node give a single bad pieces, we don't have to reject the good pieces they gave.
The severity of the attack would be in proportion to the biggest piece we could have to download.
For now just focus on merkelizing the pieces, and making sure each piece has finite size, we can do the rest after launch.

in the proofs dict we should have a flag for each thing to know if it has been updated. That way we can know exactly what to include in the batch update of the tree.

don't charge 2 different fees for making accounts. combine them.

Instead of rewarding and charging for making delete_account transactions, we should only have a reward.

maybe- Every node should keep track of the entire governance tree, that way we don't have to prove so many things.

We need a plan on how nodes are going to sync with each other. Trying to sync with everyone simultaniously is a bad strategy.


we need to make sure it is possible for channels to tell if an oracle asking a particular question was launched. That way we can conditionally send a payment depending on if it does.
It doesn't matter the number of the oracle, which question it asks matters.
Oracles should be stored by the hash of the question.
Then how are governance oracles stored? {gov_id, oracle_height}

Spending should optionally reference a recent hash. That way it is easy to spend coins only on one side of the fork, if the blockchain should split.

We need to test the case where your channel partner deletes their account. It needs to still be possible to get your money out of the channel.

We need to test the case where someone who is participating in the oracle deletes their own account. It needs to still be possible to bet in the oracle and close the oracle.

grow_channel_tx:good/1 needs to be implemented

grow_channel is bad.
To use it trustlessly, you need to make many grow_channel transaction to add a small amount of money to the channel.
We want the ability to atomically do a grow_channel transaction, and update the channel state.
So, the channel should hold one more number.
This number gets updated every time there is a grow_channel transaction for this channel.
The SPK should reference this number.
If the SPK doesn't match this number, then it is an invalid tx.
With this update, it probably becomes secure to withdraw some of the money from the channel without closing the channel.

get rid of repetition in /apps/ae_core/src/consensus/txs/spk.erl







### Needed before launch of mainnet

We need more tests of the order book for the oracle.

We need to make sure every time we take the hash of something, it is already a binary.
We don't want to have to reprogram term_to_binary in other languages.
all transaction types need to be serialized.
blocks need to be serialized.

right now when we do easy:market_match it isn't updating ssme in the channels. it should.
I set it up so the contract fails until the oracle is closed. This is probably a mistake. The contract should be able to close, but with a long delay, and the money gets distributed the same was as if the oracle closed on state bad.

It would be cool if we could simultaniously create an account and a channel with that account. That way users can get started faster. We would need a new transaction type. 2000 CHF

Maybe channels should be stored by hash too.

[AE-59] parts of the api need to be encrypted, to keep channel state private.

[CLARIFY Is it about unit testing all transactions? (via internal API)?] test transaction types in easy.

[AE-4] [AE-67] There should be a way to start the node in extra-lite-mode. So that it only downloads headers, not full blocks. 2000CHF (paid in ETH or BTC)
Naively implementing lite-mode makes it possible for an attacker to trick us into ignoring a good block. They trick us into storing a good blocks hash into block_hashes as if it was a bad block.
We need to make sure that if something was garbage collected from a merkel tree, and we try accessing the thing, it gives a different message than trying to access something that doesn't exist. Make sure we don't assume a block is invalid just because we don't have the proof of it's validity.

[AE-60] trees:garbage needs to garbage collect more trees. 100 CHF (paid in ETH or BTC)

[AE-62] We should optionally garbage collect old blocks, only keep the headers. 400 CHF (paid in ETH or BTC)

[AE-63] spk:is_improvement needs better checks. 50 CHF (paid in ETH or BTC)

Make sure delay isn't too big, and the fees aren't too high. 
Off-chain markets. 500 CHF (paid in ETH or BTC)
We need an integration test where one node is a market, and the other two nodes are traders. 

[AE-64] we need a cron like process to match trades in the markets.

[AE-65] We need to regularly check on our channels to see if either participant is running out of funds. There needs to be enough money left to cover the cost of the channel for the amount of time until you can close the channel without your partner's help.
When you are running short on funds you need to ask your partner to close the channel. If they don't, you need to start closing the channel without their help.

[AE-69] We need to also add a way for the two parties to work together to close the channel early, so they don't have to wait to do a timeout_tx. We can either make a new tx, or make channel_team_close more complicated. 200 CHF (paid in ETH or BTC)
We need a test showing that it works.
(maybe this task was already done? need to check)

[Specify more? Done?] Maybe nodes need to advertise their own IP/port combo as a peer? Right now users would need to manually add their IP/port to the list of peers.






### Things we can do after launch of mainnet

[AE-71] Secrets module seems unnecessary. As soon as we find out a secret, why not use arbitrage to update all the channels immediately?

[AE-72 - this should go to pre-launch list] maybe accessing the internal handler should require a signed request with a nonce.
The server should ignore commands that don't increment the nonce from last time.
alternatively, we could just turn on a firewall. This is simpler, but it has the drawback that commands on a local node have to originate from the same computer.

[DONE?] download_blocks:get_blocks should download multiple blocks at a time. 100 CHF (paid in ETH or BTC)

[DONE?] We need to test channel_solo_close and channel_slash and channel_timeout from easy. 30 CHF (paid in ETH or BTC)

[AE-74] Cold storage and tools. 150 CHF (paid in ETH or BTC)

[AE-75] Download blocks talk/1 seems useless. talker:talk is accomplishing the same goal. 40 CHF (paid in ETH or BTC)

[AE-77] Javascript light wallets need to be able to do all the channel stuff that full nodes do. 2000 CHF (paid in ETH or BTC)

[There is some sort of ranking already?] We need to update download_blocks so that peers get ranked, and we spend more time talking to higher-ranked peers.

[AE-78] It would be nice if there were some macros for chalang/src/compiler_lisp2.erl that did backtracking. that way we wouldn't have to think about control flow when making smart contracts.

[AE-79]
The current market design charges a 1/10000 fee on every trade. This is to protect from rounding errors.
There is a more elegant way to stop rounding errors. Set a certain maximum trade size. All orders must be measured in increments of the same size
A limitation of channels is that their output amounts are measured in integers from 0 to 10000.
Every 1 in G of the possible 10000 outputs can be valid.
A1 = amount of money getting matched from our bet,
A2 = amount of money in biggest possible bet,
B = A2 div 10000,
0 == A1 rem B
Making A1 rem B == 0 limits the possible output values of the contract, which slightly reduces liquidity. Being able to reduce the fee to zero is worth this small cost.

Maybe we should avoid revealing pubkeys until the last minute. For quantum security.
Maybe we should make pubkey vs addresses be optional,

Blocks should be serialized to be fully compressed.

### Things to do before the launch of the official Amoveo blockchain.

* the partially matched order looks completely matched in the wallet, and it looks non-matched in the explorer. it should look partially matched in each.

* there should be a way to cancel out owned assets or open orders, so if you buy something the opposite of what you have, you can make them both disappear to unlock the liquidity.

* the wallet should have some error messages:
- insufficient funds
- address incorrectly formatted
- lightning partner doesn't have a channel

* the readme should explain about public keys better

* the explorer and wallet both need an interface so you can choose which server to connect to. Maybe wallets should support multiple servers, one channel with each. We can't use hd(channel_manager:keys()) any more if we support multiple channels.

* channel manager needs a check so that we can't make bets that can't be settled do to insufficient funds.

* api:pull_channel_state might need more checks for the error banch. or maybe it will only be used for testing purposes, so we don't need more checks.

* remove ae_http/src/ae_http_dispatch_int.erl

* do not sync twice in parallel.
* sync:stop() isn't working.

* We need code so that if the market ever makes a mistake, the customers can withdraw all their money.

* We need to let people use light-node strategy to download blocks.

* the password is being recorded in the log. This is bad.

* If you use an incorrect password, there should be a useful error message.

* make sure that ae_http_app:start_external() isn't exposing files that we don't want to expose.

* when we do `make prod-build` it is preserving the keys, and replacing all the other databases. Instead it should preserve them all. only delete with `make prod-clean`. This way `make prod-restart` could update the code without deleting the databases.




* We need to redesign sharing blocks so that we don't overwhelm our partners.

consider reducing the block time below 10 minutes.
Then we could have faster trading in the markets.

It would be cool if we could simultaniously create an account and a channel with that account. That way users can get started faster. We would need a new transaction type. 

Maybe channels should be stored by hash too.

parts of the api need to be encrypted, to keep channel state private.

We need a plan on how nodes are going to sync with each other. Trying to sync with everyone simultaniously is a bad strategy.

Maybe oracles should be stored by the hash of the question. Since each question is unique.
Then how are governance oracles stored? {gov_id, oracle_height}


### Things we can do after launch of mainnet

we should use trie:garbage_leaves on light nodes to prune even more things from the trie that we don't care about.

We should optionally garbage collect old blocks, only keep the headers. 

light nodes should only download headers and a few recent blocks. They should verify blocks in parallel.

Light nodes should garbage collect almost everything from every trie they build.

Get rid of any reference to "ae", "aeternity", and "testnet".

in the proofs dict we should have a flag for each thing to know if it has been updated. That way we can know exactly what to include in the batch update of the tree.

 Secrets module seems unnecessary. As soon as we find out a secret, why not use arbitrage to update all the channels immediately?

[this should go to pre-launch list] maybe accessing the internal handler should require a signed request with a nonce.
The server should ignore commands that don't increment the nonce from last time.
alternatively, we could just turn on a firewall. This is simpler, but it has the drawback that commands on a local node have to originate from the same computer.

[DONE?] We need to test channel_solo_close and channel_slash and channel_timeout from easy.

Cold storage and tools.

Download blocks talk/1 seems useless. talker:talk is accomplishing the same goal.

Javascript light wallets need to be able to do all the channel stuff that full nodes do. 

[There is some sort of ranking already?] We need to update download_blocks so that peers get ranked, and we spend more time talking to higher-ranked peers.

It would be nice if there were some macros for chalang/src/compiler_lisp2.erl that did backtracking. that way we wouldn't have to think about control flow when making smart contracts.

The current market design charges a 1/10000 fee on every trade. This is to protect from rounding errors.
There is a more elegant way to stop rounding errors. Set a certain maximum trade size. All orders must be measured in increments of the same size
A limitation of channels is that their output amounts are measured in integers from 0 to 10000.
Every 1 in G of the possible 10000 outputs can be valid.
A1 = amount of money getting matched from our bet,
A2 = amount of money in biggest possible bet,
B = A2 div 10000,
0 == A1 rem B
Making A1 rem B == 0 limits the possible output values of the contract, which slightly reduces liquidity. Being able to reduce the fee to zero is worth this small cost.

Blocks should be serialized to be fully compressed.

* spk.erl is currently using trees when processing channel contracts. This is no good, trees are too slow. We should upgrade it to use dictionaries whenever possible.

* We need some way of garbage collecting old channels from the channels manager once the channel has been closed long enough.

* It would be cool if we could trustlessly combine a grow_channel_tx with a channel payment. This might involve a hard fork.

* reading from the hard drive can be slow. order_book can be updated to recover from errors without having to re-read everything from the hard drive.

### things to do immediately before the next hard fork of the testnet

* the merkle tree library should not pad keys so excessively before hashing them.

* we should merkelize txs and proofs before hashing them into the header.

* every trie (besides governance) should store by the hash of the key. Otherwise an attacker will make one branch of the tree too long, and proofs will have to be huge.

* constants:minimum_oracle_time() is way too low. This should be one of the governance variables.

* block_time_after_median should not be a governance variable. We should review all the governance variables.

* either implement proof of burn and proof of existence, or else remove it from the docs.

* rename oracle_shares tx type to "oracle_winnings" or something like that.

* chalang needs to use 32 byte hashes for identifying functions. 12 bytes is not enough.













### Things to do before the launch of the official Amoveo blockchain.

* raise the fees so it isn't affordable to spam the blocks.

* there are some places in the javascript light node where we aren't verifying signatures that we should  be verifying.

* when you cancel a bet, it should increase the spk's nonce. otherwise the dead bet could come back to life.

* we need a test to make sure that when we close a channel, the correct amounts of money transfer, even if we slashed more than once.

* pull channel state shouldn't cause a crash when the state is already synced.

* sync is becoming a zombie process when it cannot connect.

* the block reward should be a governance value
* what is block_time_after_median from governance used for?
* what is governance:channel_closed_time used for?
* question_delay?
* governance_delay?
* why is there a new_channel_tx fee AND a fee for making channels? We can simplify this.
* we have both a constants:minimum_oracle_time and also minimum_oracle_time from the governance. We should check if both are necessary.
* same thing with maximum oracle time.

* chalang "crash" should be called "return"

* if a smart contract runs out of gas, then the tx should still be valid. We just delete the money from that bet. This stops certain types of DDOS attacks. maybe we need to do the same thing with fail.

* we need to look at the test for options again. What if our channel partner refuses to let us add more money to the channel? Then we couldn't buy the option. There needs to be a way for just one of the participants to put their own money into the channel if they choose to.

* there needs to be an off switch on each market, so the market maker can gracefully stop his losses before too much information leaks.
- the market contract delays need to be long enough so that the contract is still live, even if the oracle takes a while to publish.

* if a customer tries closing the market early and we are at risk of losing money, then the market maker needs to keep periodically publishing evidence to the contrary, until the channel can be settled, or until someone else can be found to take on the risk of the missing channel. It works similar to combine-cancel.

* we need to be able to grow channels, and close channels from the light node.

* analyze contracts to make sure they aren't unclosable, as explained in the attacks analyzed doc.

* test lightning from the gui.

* the gui needs to make it convenient to collect winnings after a market is closed.

* maybe the gui should allow for betting in the oracle?

* outstanding_orders.js needs to be a chart, that way we don't repeat the same words over and over.

* the wallet should have some error messages:
- insufficient funds
- address incorrectly formatted
- lightning partner doesn't have a channel

* the readme should explain about public keys better

* channel manager needs a check so that we can't make bets that can't be settled do to insufficient funds.

* do not sync twice in parallel.

* We need code so that if the market ever makes a mistake, the customers can withdraw all their money.

* We need to let people use light-node strategy to download blocks.

* the password is being recorded in the log. This is bad.

* If you use an incorrect password, there should be a useful error message.

* make sure that ae_http_app:start_external() isn't exposing files that we don't want to expose.

* when we do `make prod-build` it is preserving the keys, and replacing all the other databases. Instead it should preserve them all. only delete with `make prod-clean`. This way `make prod-restart` could update the code without deleting the databases.




* We need to redesign sharing blocks so that we don't overwhelm our partners.

* consider reducing the block time below 10 minutes.
* Then we could have faster trading in the markets.

* It would be cool if we could simultaniously create an account and a channel with that account. That way users can get started faster. We would need a new transaction type. 

* parts of the api need to be encrypted, to keep channel state private.

* We need a plan on how nodes are going to sync with each other. Trying to sync with everyone simultaniously is a bad strategy.

* Maybe oracles should be stored by the hash of the question. Since each question is unique.
Then how are governance oracles stored? {gov_id, oracle_height}









### Things we can do after launch of mainnet

* merkle.js should be able to verify proofs of the trie being empty in some places.

* we should use trie:garbage_leaves on light nodes to prune even more things from the trie that we don't care about.

* We should optionally garbage collect old blocks, only keep the headers. 

* light nodes should only download headers and a few recent blocks. They should verify blocks in parallel.

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

Maybe we should update download_blocks so that peers get ranked, and we spend more time talking to higher-ranked peers.

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

* it is weird how spk_force_update22 in chalang.js calls run5. Since it's parent function is already calling the VM, it seems like we are running it multiple times unnecessarily.
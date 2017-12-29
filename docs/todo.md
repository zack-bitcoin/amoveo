### things to do for the next hard fork

* we aren't serializing slash into the channel. So we should get rid of it.
* reduce constants initial developer reward and block reward. to a fraction of a coin.
* increase constants initial difficulty.
* reduce constants initial_balance
* raise the initial governance tx fees, it would decrease the severity of many kinds of attacks. We can always lower them later. Set it up so the block reward isn't enough money to completely fill a block with transactions.
* we are putting a bunch of unnecessary zero bits before we hash a leaf in leaf.erl
* chalang signatures are double-base64 encoded. they should only be single-encoded.
* in channels tree timeout_height is unused.


### Things to do before the launch of the official Amoveo blockchain.


* spk.hrl should be renamed to records.hrl

* make sure that channels are working from the light wallet.

* in sync.erl we should start by checking each peer's version, and then ignore peers who use the wrong version.

* secrets needs to be garbage collected.

* headers:test() is broken and unused.
* keys:test() is broken and unused.
* tester:oracle_test is unused.

* change vocabulary for channels. spk is a "contract". ss is "evidence".

* there should be a refund if you close a channel early.

* there needs to be an interface to pushing the channel expiration further into the future by paying a fee.

* channel_grow tx type needs to change the expiration date, or charge a fee.

* channels need a minimum fee for opening them.

* get rid of unused min_channel_ratio

* charge the correct fee for growing a channel

* spk should have one more value. nonce_limit.

* api channel grow should send a signed spk to the server as well.

* the server should probably refuse to let a channel participate in any markets until it has enough confirmations.

* running sync:start() when we are already synced is causing some error messages.




* Use request_frequency.erl to limit how quickly we respond to requests from each ip address.

* lightning payments from the light node.

* review the rules about increasing the balance of channels. We should require a payment that make sense.
grow_channel_tx:good and new_channel_tx:good should move into the /channels directory, since they aren't related to consensus.
- there is an attack where someone makes lots of channels, then moves all their money to a small number of channels, and closes all the channels where they had lots of money. The result of this attack is that the server's money is all locked up in channels.
- ideally, we should charge based on the amount of time that the server's money is locked up. We should have the customer pay for X number of days as a minimum, and eventually we request that they pay for more days. If the customer doesn't pay in time, then we close the channel to recover the funds.
- Customers should be unable to participate in any contract that doesn't settle in the time alloted for them.
Market contracts need some sort of default, so they can be closed within the limit. The default should probably be that the server wins, this way the customer can have the freedom to set up the bet with whatever time constraints they want, at their own risk. It is a sort of "I cut, you choose" protocol.
- we should have a constant in the config file be "time_value", and we use this to calculate how much it costs to have the server's money locked up for a period of time.

* raise the fees so it isn't affordable to spam the blocks.

* make the pubkeys more convenient for copy/pasting. It would be nice if we used compressed pubkeys instead of full pubkeys. Maybe we should use the base58 library, or the pubkey checksum library.
Maybe encoding the pubkeys should happen at the wallet level, not the node level.

* there are some places in the javascript light node where we aren't verifying signatures that we should be verifying.

* when you cancel a bet, it should increase the spk's nonce. otherwise the dead bet could come back to life.

* pull channel state shouldn't cause a crash when the state is already synced.

* we need to look at the test for options again. What if our channel partner refuses to let us add more money to the channel? Then we couldn't buy the option. There needs to be a way for just one of the participants to put their own money into the channel if they choose to.
Oh, we should have our partner sign a transaction that allows us to put money into the channel, and we can choose whether or not to sign it in the future.
So it is important that a channel_grow transaction ignores the nonces of the two accounts that sign it.

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

* We need code so that if the market ever makes a mistake, the customers can withdraw all their money.

* the password is being recorded in the log. This is bad.

* If you use an incorrect password, there should be a useful error message.

* parts of the api need to be encrypted, to keep channel state private.

* Maybe we should make a way to run internal handler commands externally. Add a new command to the external api. To call this command, you need to sign the info with your private key, and it has a nonce inside that needs to have incremented from last time.








### Things we can do after launch of mainnet

* merkle.js should be able to verify proofs of the trie being empty in some places.

* we should use trie:garbage_leaves on erlang light nodes to prune even more things from the trie that we don't care about.

* We should optionally garbage collect old blocks, only keep the headers. 

* light nodes should only download headers and a few recent blocks. They should verify blocks in parallel.

Get rid of any reference to "ae", "aeternity", and "testnet".

in the proofs dict we should have a flag for each thing to know if it has been updated. That way we can know exactly what to include in the batch update of the tree.

 Secrets module seems unnecessary. As soon as we find out a secret, why not use arbitrage to update all the channels immediately?

Cold storage and tools.

Download blocks talk/1 seems useless. talker:talk is accomplishing the same goal.

Javascript light wallets need to be able to do all the channel stuff that full nodes do. 

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
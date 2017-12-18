### things to do for the next hard fork of the testnet


* make sure we are using every governance variable between 0 and max exclusive. Don't have empty spots.

* set the governance tx fees to reasonable values to deincentivize miners from filling up the blocks with junk data.




### Things to do before the launch of the official Amoveo blockchain.


* Use request_frequency.erl to limit how quickly we respond to requests from each ip address.

* Syncing should split the process of headers and blocks. If you try to mine, it shouldn't start mining until you download almost all the blocks for the headers you know about. 

* the config constant "garbage_period" is unused. We should review the garbage collection mechanism to see if this constant is needed. We also  need to know if this constant limits any other contants, in particular "fork_tolerance".

* review the rules about increasing the balance of channels. We should require a payment that make sense.
- there is an attack where someone makes lots of channels, then moves all their money to a small number of channels, and closes all the channels where they had lots of money. The result of this attack is that the server's money is all locked up in channels.
- ideally, we should charge based on the amount of time that the server's money is locked up. We should have the customer pay for X number of days as a minimum, and eventually we request that they pay for more days. If the customer doesn't pay in time, then we close the channel to recover the funds.

* rename oracle_shares tx type to "oracle_winnings" or something like that.

* raise the fees so it isn't affordable to spam the blocks.

* make the pubkeys more convenient for copy/pasting. It would be nice if we used compressed pubkeys instead of full pubkeys. Maybe we should use the base58 library, or the pubkey checksum library.
Maybe encoding the pubkeys should happen at the wallet level, not the node level.

* there are some places in the javascript light node where we aren't verifying signatures that we should be verifying.

* when you cancel a bet, it should increase the spk's nonce. otherwise the dead bet could come back to life.

* pull channel state shouldn't cause a crash when the state is already synced.

* sync is becoming a zombie process when it cannot connect.

* we need to look at the test for options again. What if our channel partner refuses to let us add more money to the channel? Then we couldn't buy the option. There needs to be a way for just one of the participants to put their own money into the channel if they choose to.
Oh, we should have our partner sign a transaction that allows us to put money into the channel, and we can choose whether or not to sign it in the future.

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

* the password is being recorded in the log. This is bad.

* If you use an incorrect password, there should be a useful error message.

* make sure that ae_http_app:start_external() isn't exposing files that we don't want to expose.

* We need to redesign sharing blocks so that we don't overwhelm our partners.

* parts of the api need to be encrypted, to keep channel state private.

* We need a plan on how nodes are going to sync with each other. Trying to sync with everyone simultaniously is a bad strategy.

* We should make a way to run internal handler commands externally. Add a new command to the external api. To call this command, you need to sign the info with your private key, and it has a nonce inside that needs to have incremented from last time.

* the explorer should display in coins, not satoshis.







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
### things to do for the next hard fork


* maybe embed recent news message into genesis block.
  -maybe we can reuse #block.trees for this.



### changes to code before official launch of Amoveo mainnet blockchain


### Other hard fork ideas

Maybe we should add a governance variables for each opcode in the VM. To be a gas price per opcode.

### For next hard fork




### Things to do

* deleting accounts from the light node might have problems.

* atomic swap protocol

* main.html from the mining pool should say the total number of miners, and the total outstanding shares.

* write docs for making channels in the light node.

* fix the race condition where it says `potential block died!`.
- maybe it is already fixed?
- it seems like there is a memory leak. normally uses 60 mb for Amoveo, but it slowly rose to 150 mb.
- sometimes we are pruning data that we should not be pruning. I only happened on one node, not all of them. restarting from 0 fixed it.

* check the case where someone sends a good headers with a bad block. Don't ignore the good block after this happens.

* don't delete the blocks so easily. they are useful for recovery.

* miners need instructions on making pubkeys.

* test_txs(15). channel slash isn't being automatically created.

* It is not clear in the docs on github how to update values in the config file. 

* if the response from {give_block, Block} is not base64 encoded, then it freezes us from pushing the new block to peers. We should probably decode it manually so that we can handle errors better.

* potential block:new_internal2 can use headers:top_with_block instead of the slow block_to_header.

* test the case where we know about more headers than blocks, and we want to recover the network by mining a new version of history.

* it seems like if we are aware of txs from future blocks, it can prevent us from verifying those future blocks.

* sync gen server is getting a too-full mailbox, and it is filled with unnecessary repeat data.

* optimize the protocol for trading peers and txs. Only send txs and peers that they don't know about. Trade these lists less frequently, right now it is too much bandwidth.

* decrease how often miners try to sync.

* we are wasting some time syncing with ourselves.

* it isn't switching to normal mode well !!!!!!!!!!!!!!!!!!!!!!!!!
maybe we should switch back to the original idea. If it finds less than 1 block per 5 seconds in the last 2 minutes, then switch to normal mode.
- update docs getting-started/turn_it_on.md

* measure the rate at which blocks have been found in the recent 2 minutes. if the rate is < 1 block per 30 seconds, then switch to sync_mode normal.

* in the mining pool, we need to make the cron task into a gen_server so that it wont crash.

* if a peer refuses the blocks we send them, then blacklist them.

* it is confusing how we have sync:start/sync:stop and we also have sync_mode:quick/sync_mode:normal.
Can't 1 flag do both things?

* instead of downloading a certain number of blocks at a time, we should have a certain number of byts we download at a time.

* if you make several txs in a row, they don't all get included in a block.

* fix compiler warnings. unused variables and such.

* delete unnecessary messages

* we should probably delete and rebuild the configuration files every time we build the project. That way you don't have to ever manually delete them.

* limit how much bandwidth any individual IP can consume.

* decrease data volume requirements.

* if you try turning the node on while it is already on, it should at least give you error warnings. At best it should also refuse to corrupt it's database.

* the documentation needs to be clearer that you are not supposed to run the software as root. you are supposed to make an account.

* the explorer should say how many headers, and how many blocks the node it is connected to has.

* maybe we should use pkill instead of killall in c-miner clean.sh

* the light node should automatically know whether you need a spend_tx or a create_account_tx.

* start out with more default peers.

* do txs get dropped from the mining pool tx_pool if someone else mines a block?

* the light node should have tabs, so it doesn't display so much at the same time.

* rethink the process of creating blocks and mining. It seems like race conditions might happen here.

* looking up blocks to push is too slow. we should look them up in reverse order to be faster.

* lightning payment api is not verifying data before using it. We should fail fast instead. Review the rest of the external api for the same problem.

* if the node is already running, then `make prod-restart` should do the same thing as `make prod-attach`

* `sync:stop().` should be called before `api:off().` when shutting down. put this in the documentation somewhere useful.

* config fork toleranace and config revert depth should be the same thing.

* documentation for sync_mode.

* in block_absorber.erl we are spawning a process that has to filter some txs. We can make this faster n**2 -> n*log(n).
- additionally, we should only do this calculation when in normal mode. If we are trying to sync blocks quickly, we can skip this calculation.

* in spk.js there is a spot where we should be verifying signatures, but we are not.

* spk get_paid and spk apply_bet should probably incrase the nonce by more than 1.

* When syncing we are building a potential block at every height. This is very innefficient. Instead we should download the entire history before building potential blocks.

* the mining pool should never spam the full node. They are on the same machine.

* rethink syncing. If you have more headers than the server, it fails to download blocks.

* The light node should allow for betting in the oracle. 

* when your bets get matched, the ss gets displayed in the browser. We should probably display more information along with the ss, so users can more easily tell if they need to use the smart contract enforcement mechanism.

* more tests of the attack where someone generates tons of fake peers and adds them all to the list.
- maybe we should limit how many peers we are willing to download from any one peer.
- There are some peers hard-coded into the core software. If these peers are not in our peer list, we should occasionally check to see if we can link with them again

* verify that the light node will not make a market smart contract where the server might not have enough funds to pay.

* if the peer isn't accepting blocks, then do not blindly give it more blocks.

* the light-node is memorizing the server's pubkey too early. If you change the ip you are connecting to, it should change the server's pubkey we store in ram as well.

* the light node fails badly. It should give useful error messages.

* The password is being recorded in the log. This is bad.

* it should be more obvious that miners need to insert their pubkey into c_miner and javascript_miner.

* the c-miner should have an easier way to decide which node you will connect to.

* We need to prune bets and orders.

* the light node should have an interface for encrypting and decrypting messages. It should have an interface for signing messages, and checking signatures.

* javascript light node should give an option for extending the time limit in channels. there is an api for paying the server already. modify this slightly.

* outstanding_orders.js needs to be a chart, that way we don't repeat the same words over and over.

* we need to test out the different formats for "true" and "false" in the javascript light node.

* go through every case in the light node where we do a variable_public_get. Make sure we verify the response as much as possible. Do not blindly sign or store anything from them for example.

* in sync.erl we should start by checking each peer's version, and then ignore peers who use the wrong version.

* secrets is leaking data.

* headers:test() is broken and unused.
* keys:test() is broken and unused.
* tester:oracle_test is unused.

* change vocabulary for channels. spk is a "contract". ss is "evidence".

* there should be a refund if you close a channel early. The refund should be enforced by a smart contract. It is important that this smart contract's nonce does not increase with time, otherwise the contract can be slashed forever.

* running sync:start() when we are already synced is causing some error messages.

* Use request_frequency.erl to limit how quickly we respond to requests from each ip address.

* make the pubkeys more convenient for copy/pasting. It would be nice if we used compressed pubkeys instead of full pubkeys. Maybe we should use the base58 library, or the pubkey checksum library.
Maybe encoding the pubkeys should happen at the wallet level, not the node level.

* pull channel state shouldn't cause a crash when the state is already synced.

* there needs to be an off switch on each market, so the market maker can gracefully stop his losses before too much information leaks. or does there? this should be thought out more.

- the market contract delays need to be long enough so that the contract is still live, even if the oracle takes a while to publish.

* analyze contracts to make sure they aren't unclosable, as explained in the attacks analyzed doc.

* the wallet should have some error messages:
- insufficient funds
- address incorrectly formatted
- lightning partner doesn't have a channel

* the readme should explain about public keys better

* If you use an incorrect password, there should be a useful error message.

* parts of the api need to be encrypted, to keep channel state private.

* Maybe we should make a way to run internal handler commands externally. Add a new command to the external api. To call this command, you need to sign the info with your private key, and it has a nonce inside that needs to have incremented from last time.



* calculating block_to_header is too very slow. Which means calculating the hash of a block is slow too.
* We should store the hash of the block along with the block, that way we don't have to re-calculate it more than once. When sharing blocks we can use this hash to quickly ignore blocks we have already seen, but for a block to be considered valid, we need to check at least once that the hash was calculated correctly.

* merkle.js should be able to verify proofs of the trie being empty in some places.

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

* We need some way of garbage collecting old channels from the channels manager once the channel has been closed long enough.

* reading from the hard drive can be slow. order_book can be updated to recover from errors without having to re-read everything from the hard drive.

* it is weird how spk_force_update22 in chalang.js calls run5. Since it's parent function is already calling the VM, it seems like we are running it multiple times unnecessarily.

* in tx_pool_feeder absorb_async we sleep for a little time. This is a hackish solution. There is probably a way to use an additional gen_server to make it efficient.

* maybe governance history should be stored by default.

* we need configuration options to decide which parts of the historical data you want to keep.
* it should be possible to store a shart of a tree.

* an api for downloading blocks in a compressed format.
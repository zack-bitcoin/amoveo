
### things to do for the next hard fork




### Other hard fork ideas

Maybe we should add a governance variables for each opcode in the VM. To be a gas price per opcode.

Maybe question oracles don't need so much (any?) initial liquidity like governance oracles do.




### Things to do

* sync_mode:normal and sync_mode:quick should be available from the api.

* share new block headers to all the mining pools.

* return the hash of the tx when you publish a new tx.

* we should have more rules for ignoring bad peers. If they send the same request too often, or if they send invalid data more than 10 times per minute. 

* tx are still being dropped.

* during DDOS, sometimes nodes end up dropping all their peers, and are then unable to sync. We should refuse to black list the hard coded peers.

* similar to the oracle lookup tool, we should have a governance value lookup tool in the light node.

* built in translation to the light node is a bad idea. Google chrome has translation built in anyway.

* check that txs don't get dropped when a block is orphaned.

* is merkle spk.js looking up the merkle proofs for the wrong information??

* the market.fs contract has a problem. Is the expiration date output of the contract relative, or absolute? I am not consistent.

* we should test the case when there are multiple partially-matched trades in the order book simultaniously.

* when you make a channel in the light node, there should be a way to look up how much the server charges before

* the light node should have an interface for encrypting and decrypting messages.
* display oracle data verified by merkle proofs.

* harden mining pools against attack.
- Consider hiding some of the API behind a fire wall.
- put a limit on how many blocks you can download with a single request.
- when syncing you should simultaniously download blocks from multiple peers to spread the load.
- limit how many bytes we serve to any one IP per 10 seconds.
- as well as downloading X number of blocks at a time, we should enable downloads of M megabytes of blocks at a time.

* if you haven't done `sync_mode:normal().` and you try to publish a tx, then there should be some sort of error message. Maybe connect it to the tx signing function.

* test downloading the light wallet. There are reports that it is not connecting to a full node easily.

* if you have the wrong private key loaded into a light node and try signing a tx, it should give a useful error message.

* port "8080" defined in more than one place.

* documentation for governance oracles
You have to set it between 1 and 50 inclusive.
if the governance variable is above 100, then it is a percentage.
If the governance variable is below 100, then it is an integer value.

So if governance is currently 40, and you change by 30, you can go to 10 or 70.
If the governance is currently 1000, and you change by 30, you can go to 1300 or 700.

- true means higher and false is lower, right?


* ubuntu 18.04 compatibility.

* order_book:match() should have a timer so we only run it every 3 minutes. Otherwise it is wasting cycles while we are syncing blocks.

* block:check/1 should be split into 2 functions.
- first we want to verify that a block could be valid by looking at that block alone.
- second we want to compare the block to the previous block's state, to make sure the block is valid in context.
- the first step is computationally expensive, and it doesn't touch the hard drive. So we can parallelize step 1 with itself. We can verify multiple blocks simultanious to use more CPU on our machine and sync faster.
- block_absorber should only do the 2nd step of block check.
- block_organizer:add should do the 1st step in parallel.

* in proofs:txs_to_querys2, in the oracle_close branch, we are using Accounts and we should be using Accounts2.

I think he means put an entry in /etc/hosts for amoveopool2.com such that it points to your pool... but that would only work if your pool accepted /work on the end of the URI
* update pool to accept /work
* make a script to take advantage of the /etc/hosts trick.



* maybe potential_block.erl should save the 2 most recent blocks, so if a nonce doesn't work on one, we can try the other.

* the market user interface should say how many blocks until the next batch.

* prevent the creation of markets with batch periods that are too short to be secure.

* add the hash of the github commit in the explorer.
% git ls-remote | grep HEAD

* atomic swap protocol

* main.html from the mining pool should say the total number of miners, and the total outstanding shares.

* check the case where someone sends a good headers with a bad block. Don't ignore the good block after this happens.

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
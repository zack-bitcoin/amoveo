write a blog post about using futarchy to measure the effectiveness of military spending.


hard update idea to be researched:
We want to be able to partially match an open channel offer, and the rest of the channel offer is still valid, it can be matched by other people.
This will probably be important inside sortition chains as well.

consider getting rid of modes in otc_derivatives, move it to home page.

in otc_derivatives.html, some tool to automatically make a bet on an oracle hash, maybe some other preset bets, like a binary bet on whether bitcoin will go up or down in a time period. We can do the front-running protection where both dates are in the future.

for the bet resolution process, it would be nice if the page automatically looked up the result, so the user doesn't need to do it manually.

for pre-set bets, it would be nice if it could suggest a price based on the current veo/btc price or whatever they are betting on.

use the same method to select the price to trade, and for resolution.

in otc_derivatives.html, when you make a channel offer, there should be a button to immediately publish it to the p2p derivatives server, without having to open that page.

a link from the p2p derivatives page back to the light node otc_listener page. that way it is more obvious how to accept a trade.

test that the trade auto-populates correctly, when you link from the p2p derivatives page.




remove legacy code from otc_finisher. we no longer use channel data, we dont use function start1(). we don't use start_button


in otc_listner: 
maybe we should reconfigure it to be like “if X is true, you win Y veo. if not, you lose Z veo”
then you could get rid of most of the text there


simplify OTC_listener, look at the pic from Flinstone in /home/zack/Pictures/
* have a "see more details" button to reveal all the extra details about the smart contract.

rewrite otc_* in the light node



needed for cold storage https://github.com/zack-bitcoin/amoveo/issues/184
multi-tx from the light node


hard update to prevent an attack.
The attack mines a valid looking header, but doesn't reveal the block. 
So the rest of us aren't sure if it is better to build on that header, or to ignore it and build from the highest known block.
We could also include a Merkel root of the txs from the previous block, but they should all be salted with like a single 0 byte each.
That way, it is impossible to mine a valid block without knowing the txs from the previous block, so this will prevent any miners from building on the attacker's header.
So then this attack becomes the same as the selfish-mining attack, which we already know does not work.





syncing blocks in reverse order.
* checkpoints.




video about how to do betting with Amoveo. preferably with 2 narrators, one a beginner, and one with experience.



would be nice if there was a way to combine settling a binary derivative contract with a small payment. to incentivize them to settle earlier.






Now is 83342
find out which block the channel got created in
BFD1dYbhVe8vEzCmtu/70m+lPKXzDoKIHlNjGiJCXYeGapadChVENendo8G1XskHKfMD8G18kWdrJ2r9ok/iFV8=

in p2p_derivatives/channel_offers_ram:valid/1
the part that is like `true -> true`, we should check if the account's balance is big enough for the contract to be valid.


p2p derivatives web page should allow direct links to look up oracles or contracts.


update syncing blocks to use websockets.
That way nodes don't need to open their firewall in order to receive blocks.



restore the proof-of-existence txs, they are needed for hashlocking with sortition chains.

GPU verifier for the https://github.com/zack-bitcoin/vdf_calculate RNG, needed for sortition chains.

the tx types that will allow for sortition chains to exist.



consider setting up a backup system for pulling headers, if your IP isn't on the list.

explore alternative things for futarchy to optimize for:
sharpe ratio.
return/volatility

just maximizing for price isn't good. for example a 50% chance of 3x and 50% chance of going to 0 has a positive expected value.


fix the centralized betting tool.
Bets aren't being matched because when we run the smart contract it doesn't result in a delay of 0.
this is the old broken tool for matching -> channel_feeder:bets_unlock(channel_manager:keys()),
in order_book:match_internal2 we are calculating all the orders that are getting matched. We should collect all this information so we can update exactly those contracts to be matched.






Potential block should keep track of the 2 most recent version of the work, so that miners still working on the previous version aren't wasting their effort.




new_oracle binary oracle is not working.



chart to visualize historic difficulty

new_oracle.js governance_futarchy_oracle could suggest block heights automatically


update the scalar and binary market contracts to use the new strategy for generating oracle ids.
we can generate all 10 oids inside of chalang, so we dont have to embed 10 raw oids.
test out scalar oracles from the light node before pushing anything.

do a hard update to enforce the new way of generating oracle ids.

update the smart contracts to handle oracle that do not yet exist.

update the light node to handle contracts for oracles that do not yet exist.

check that p2p derivatives explorer can handle contracts for oracles that do not yet exist.



update p2p derivatives explorer to keep a record of which contracts expired unmatched, vs which were matched. This will make futarchy easier.




once the hard update activates at 76200
update the light node for the new channel close tx type.

update the light node to use the new simpler binary contract for p2p bets, but use the old one for centralized betting with the server.

write a simpler scalar contract.

update the light node to use the new simpler scalar contract for p2p betting.

make sure the light node still works for bets in the centralized market.

teach the light node chalang about the new destructive comparison opcode, and the new ways of encoding integers.

soft update. require that the oid of each oracle was deterministically generated from the height when trading begins and the question being asked.
* also update the light node to enable trading of derivatives on oracles that do not yet exist.


the new tool to execute a command when we find a block is not working, because currently the block isn't updated until you try pulling the data from it.



idea to improve the oracle mechanism
* block any bets that would leave the oracle in a state where less than 1 veo of volume of bets is sitting in the order book.
the advantage is that we could reduce the initial liquidity to 0, so it would be cheaper to make oracles.



add to chalang:
* load the next 1 byte as an integer
* load the next 2 bytes as an integer
* 50 or 100 opcodes that each load a single integer from 0-99 onto the stack. So we can use 1 opcode to load a small integer.
* we need to set up a soft update to activate this

add to chalang:
* grab arbitrary byte from a (binary or (an integer, which is interpreted as a 4-byte binary.))



try pushing a tx from the light node in chrome on a mac.


having trouble making channel_solo_close from the light node to close our channels.

The light node should store the entire compiled contract. That way, even if the rules for how to build the contract should change, our old signatures are still valid. And so we can update how the contract is built without invalidating existing contract signatures.

Make transactions to close the 5 channels in ~/oracles.md, and publish them somewhere publicly so that the other participant can close these channels.

rewrite the scalar market smart contract like market2.fs
update the light node to use these better contracts.



teach the master branch not to have zombie nodes trying to download non-existance blocks.



like, it should say somewhere that bad question means you get your money back


also, I think “you win if veo/stablecoin goes up” is the most intuitive way, when veo/stablecoin is a number quoted like 92 and not 0.01085

veo/stablecoin and stablecoin/veo labels might be backwards.

update futarchy documentation:
I think it is much better to use a % change in future interval
like, % change from block where reward increase goes thru until like 1000 more blocks or something
and then there isn’t the free option dynamic as much anymore

experimental version seems to have some race condition while syncing. alarmingly, you need to do `block_hashes:second_chance().` in order to get it to continue syncing after the error happens.


* add randomness opcodes to sortition chalang, as documented in opcodes.md load_seed and get_entropy.





We need to get the randomness into chalang, so we can make these sortition merkel proofs.
I guess we should choose a block hash to import as a binary?

Ideally we would use the block hash are the seed for a deterministic random number generator, that way we can have more than 256 bits of entropy from a single block hash.

So I guess I should use an erlang library for this, and add some chalang words to for:
1) load a block hash onto the stack.
2) load a seed into the random number generator
and then every time we call rand_bool, it can use the random number generator to make the decision.


teach the light node to use ctc2 instead of ctc.



Why did the light node follow it's own fork briefly?


sometimes the light node generates channel_team_close txs where one of the accounts spends more money than they have in the channel, so the tx is invalid.
- unable to reproduce.


amoveo light node channel team close should handle the case where the channel participants send their channel states to each other.
Either there should be a warning if you use the wrong channel state, or it should use the information to accomplish the goal anyway.

when you make a channel team close offer, give an option to save to file.

display the channel ID as much as possible in light node for p2p derivatives.


amoveo light node solo-close could be having problems.



lower the cost of question oracles.


figure out prob-payments from blitzkrieg.


figure out channel factories.





sometimes when a block syncs, the full nodes aren't clearing out their entire tx pool, and they keep an invalid tx. This could potantially prevent them from finding blocks, so it could be a serious error


if you block other nodes from talking to you, eventually you drop all your peers. This could be related to the memory leak causing there to be too many threads trying to sync at the same time.


we should be able to create channels for accounts that do not exist, and they are created when the channel is closed. for prob-channels.


* syncing blocks in reverse order.

* probabilistic payments.

every tx where you need a gov variable to calculate the new balances, put that gov variable into the block meta.


if `sync_mode:normal().` hasn't been done, and you try making a tx, there should be a useful error message.


rename the tx type to oracle_unmatched.


if we don't export our IP, do the other nodes blacklist us?
Why does it stop syncing eventually?



Why did the light node follow it's own fork briefly?




gov variable in block meta is in the wrong format.


if a node sits long enough, we end up with lots of threads downloading empty lists over and over.
it is requesting above the top height that anyone has



light node otc_listener.js "someone already did this contract. you are too late"
we should put a message to the screen to indicate this.

Some confirmation message when you accept a contract.

Stepan lost the channel state for a channel. What can we do to help him?

better error message if you try to look up a channel that doesn't exist in the light node.





tx_scan is failing, make some better tests.

to calculate the delete amount correctly,
* block:get_txs_main needs to be implemented



do a second pass, check if any delete_acc_tx has an amount too low.


we need a better warning system if a governance value is being changed.
message loloxian when it is ready.


we should probably store blocks and meta data seperately.

oracle_winnings and oracle_unmatched txs from the light node.



store the highest hash with each page of compressed blocks, since this makes it easier to organize the blocks and resync them later.
We should store the top hash of a page of blocks under the key "top".


issue with channel_team_close2
blocked because packer doesn't know about the key.
We can't fix packer directly, because some nodes would freeze.
1) mining pools should do a soft fork to block ctc2
2) we fix the packer library.
3) we give everyone a week to update dependencies.
4) we schedule a date to simultaneously turn off the soft fork in all the mining pools.
* currently here.
5) clean up now unused soft-fork code.




move all records to records.hrl . Many developers are exposed to the datastructure first, and then search for the file with the keys second. they have difficulty knowing which file to look in to find the keys.






futarchy markets:
* lower block reward
> if the block reward is below 0.3 veo, return bad. else return the price of USD in VEO.
> if the block reward is above 0.3 veo, return bad. else return the price of USD in VEO.
* lock the block reward with a set halvening schedule


set up a testnet



merkel tree memory leak for miners.



configuration option to not store any blocks.


automatically adding your IP to the list of peers is failing.


replace many dictionary data structures with ets.


otc_listener should display the channel ID.
* we did this, now we need to test it and then push to github.


make a javascript tool for managing channel states.
It should tell you which channels are ready to be closed, and display a chart for how much money is in each contract, how much longer until it can be closed, and store it all in a single file.


glossary long-veo/stablecoin on otc_derivatives and otc_listener


start closing some oracles


scientific notation oracles.
How about we combine a  binary and scalar oracle. so you can make a new binary oracle, and combine it with an old scalar oracle, to make a contract that either returns $0 or $200 of veo.
P = the amount of veo in $200 from 0 to 10; if A happens return P * 1024 / 10; if A doesn’t happen return 0


in the light node, when we look up oracles, we should verify that the hash of the question tx matches the hash stored in the merkel tree.


sharding.



in the light node update from bigInt js library to the BigInt built in the browser.


another button in otc_derivatives, this one for using oracles to make an inverse stablecoin.
So if the price of amazon shares in USD increases 5%, then your inverse stable shares will have decreased 5%.
The only difference is that whatever price the user types in as the initial price, replace that with (limit_max - the_price_they_entered). because everything is flipped vs a normal stablecoin.


people want shorts in holo/rvn/abbc


otc_listener should display the channel ID.


new_oracle should make some standard format for oracles so that we can easily parse the oracle question and other tools can say if it is a USD stablecoin, or a BTC stablecoin, or whatever.
It could translate block heights to dates.
new_oracle page simplification.
We only ask for: 3-letter ticker, maturity data, and max price, short/long
Then the oracle question will be more standardized, so we can parse it easier at other steps.

Also standardize inverse stablecoin oracles, so the interface for making bets can be simpler.

Maybe change the name on the "stablecoin" button in otc_derivatives. It is for more things than just stablecoins.
"scalar - simplified" and "scalar - advanced" could be good names on the buttons.


The p2p derivatives pages have too much info. remove everything that we do not need.

add an atomic swap feature to api. look at the decred atomic swap example.

channel team close should have a limit, so it needs to be posted in the next 10 blocks to be valid.
To prevent people using it as a free option.
* being written in channel team close tx 2.




reduce orphan rate on small pools.

a tool to review the state of your active contract.

stablecoin interface in the light node should accept bets in either direction.

instead of displaying the oracle upper limit, it should just have an error message and block you from continuing if it can't load the upper limit.

maybe we need an extra confirmation so that with the scalar interface you don't accidentally make a bet where your partner puts nothing at stake. display the same text from otc_listener in otc_derivatives when making a contract.

add salt to amoveo smart contracts for privacy

moving bets to a direct path.

a website for listing channel offers.


maybe it was a mistake to set up int_handler.erl to always return 2-tuples that start with "ok".


What if 2 people try to match to create the same channel? is the error message useful?


in otc_finisher, if the oracle is already closed test making a ctc to close the channel.


we should use nlocktime on all the txs so that it isn't possible to profitably undercut and include future txs at an earlier height. It should be impossible to move any tx into a block height from earlier than the tx was made.
* if we add maturity times to block rewards, then we can't use the block reward for anyone-can-pay txs.


hard update to support something similar to anyone-can-spend tx types in bitcoin. This is an important tool so that miners can share windfalls, that way miners never have an incentive to undercut each other's blocks.
Thank you to Fernando Nieto https://twitter.com/fnietom for explaining this solution to me.




option to customize the delay when making p2p oracles.


refactor the chalang market and oracle a lot.


Maybe tx fees should be put into a pool, and the pool distributed to the miners over time.
This way we can safely make the block reward lower than the tx fees.


oracle bets should reference the previous block's hash, that way you can't reuse many in a reorg attempt.

api to check what a scalar oracle will output if no more bets are made.

test solo-closing the channel from the p2p derivatives node. make sure the correct amount of money is moved.

test the case where someone pays a higher fee.
* the limit order trick for new channels probably does not work in the long run, because you can make off-chain tx fees.

unmatched:dict_significant_volume, we should probably be checking if manyOrders is >1, not 2.

would be nice if in otc_finisher the same file upload spot could accept either channel states or trade offers, so that the interface can be simpler.


api:oracle_bet should have a useful error if you try and use a floating point value

light node should tell you if you have insufficient balance to ask the oracle a question.

figure out what went wrong with the stablecoin contract to Evan Pan.

when you make contracts in the p2p tool, we should check that you private key matches the address in the channel of the contract.

in otc_finisher.js we need to make it clearer how to write the final price. people confuse veo/usd with usd/veo.
We should have some confirmation saying what portion of the veo goes to each party.

similarly, if you accept a proposal from someone, it should say something about how much of the money goes to each party.


We should sum up input money and output money from each block to double-check that there is no counterfeiting.

combinatorial chalang contract tested. combinatorial_market.erl written.

in the light node, the contract offers should have time limits other than 100.

in otc_finisher, it should display the details of whatever bet you have made.

in otc_finisher, we never need to save the channel state, so get rid of that button.

consider paying the exchange digitalprice.io to list Amoveo.

We need a theory of the flavors of trust.
Between 0 trust and 100% trustlessness, are there discrete layers, or is it continuous?


Now we have tools for signing and verifying on spks.
spk:sign/1
spk:unwrap_sig/1 %this converts the new kind of signed spk into something that can be verified the old way, it leaves the old kind of spk unchanged.

* replace every case where we sign an spk with spk:sign.
* replace every case where we check a spk's signature with testnet_sign:verify(spk:unwrap_sig(Stx)).



add endpoints to the amoveo api to access other amoveo services that could be running on the same machine. use white-lists, don't let them connect to internal api or run anything dangerous.


simpler way to customize port instead of 8080



### Other hard fork ideas


we should have a time limit in channel team close txs to prevent the free option problem in some cases where they want to close the channel early.


Maybe block rewards should be locked for a week to prevent P+epsilon attacks against the consensus mechanism.
Maybe locking for less than a week would be enough.
Calculate how much time the coins would need to be locked up for.


free option problem when closing a channel early with CTC.
Right now we only use the nonce of acc1.
So there is no free option if acc1 signs first, since acc1 can make another tx to make the ctc invalid if it isn't published soon enough.
We should update CTC to accept the accounts in either order, that way acc2 could sign first by listing themself first.

* we need to add more information to all the txs. when a channel is closed, it should say how much money is going to each participant, and there are many other cases.

* we should give a reward for closing oracles.

* merkel proof and verification code for txs in blocks. and rewrite it to javascript. That way we can prove if a tx has been included in a block.

* the oracle should say the sum of open bets, otherwise it is so complicated for light nodes to request a proof of this information.

maybe governance oracles should have a minimum amount they need to be changed by. otherwise an attacker can block an oracle from being made by keep making the same oracle to only change 1%.

Maybe we should add a governance variable for each opcode in the VM. To be a gas price per opcode.


maybe channel_team_close_tx should have a negative fee. As a reward for deleting the channel.
We could raise the fee for opening channels, and the reward for closing them.
This would prevent attacks where the attacker opens too many channels, and tries to close them all in too short of a time period.

* rename oracle_bets and orders. (is this a hard fork??)



### governance ideas

* we should probably lower the block size limit more.


### Things to do

tree_data:idut2/4 needs to be updated to support light nodes.




find out where fees are coming from if you keep forming and canceling bets.

some oracles can never be closed. we should stop returning them from any queries of the oracles.

maybe we should do block_hashes:second_chance(). on restart, because it makes usability easiler without significantly impacting the cost of restarting.

add a function to api instead of api:orders/1. so we can look up the unmatched bets in one oracle.
figure something to replace the oracle_bets endpoint in ext_handler, it should probably be removed.

remove mentions of "testnet"

chalang should be aware of the block time.
use the block time in a new version of the market.


* people should be able to use a light node to make channels with custom bets directly between each other.
https://github.com/zack-bitcoin/light-node-amoveo/issues/4

* remove depreciated javascript code.

* untrusted third parties who hold channel data and publish it if a channel is closing wrong.

* if channels mode is turned off, then don't share your pubkey from the api.

* when we first sync a node, it should automatically try to pull headers from a bunch of different peers. This way we are more likely to find out about the highest header.

* torrent of the light node.

* confirmed_root has the constant "confirmations". it needs to be combined with something from the configurations.

* we should blacklist peers who send us invalid blocks. Ignore any messages from them.

* we rarely change any governance value, so why does the pointer increase so much?

* teach the light node to scan multiple servers to identify the version of history with the most work.

* light node should know how many bets are outstanding for it's account in oracles.
*light node needs to be able to look up the volume of oracle bets. (or at least put it on the explorer for now.)

* fix variable and function names for readability.


* there is a bug. channel data gets stored into the channel manager, even if the tx didn't get produced.

* Once a share is matched, then we know exactly how much veo it needs. So we should simplify the contract and extract the excess veo to be used in other smart contracts in this channel.

* We need a way for pairs of people to write a custom CFD contract for a single channel between them using only the light node.

* the integration test should include removing old bets from the channel state. javascript can do it, so erlang should be able to as well.

* in ext_handler:new_channel/3, we accept channels made in either direction, is this really secure? Make sure we don't assume the direction in any other step.

* add a note to this hard fork that full nodes running markets will need to close all of those markets first before doing this update. Channels do not have to be closed, they can contain old and new contracts at the same time.

* in channels.js we need to give the user a chance to confirm that the period, server_pubkey, and expiration are all the expected amounts. Along with anything else from ext_handler:market_data



* transactions are being dropped ??

* voting in the oracle from a light node.

* we need to display more oracle data:
- total matched bets of each type.
- total unmatched bets.

* partial refund for closing a channel early.

* make it more clear what the cost of forming channels is.

* if you try making a channel with all of your veo, then the tx gets dropped, but the server still stores channel state. So you are unable to make a new channel. We need to prevent this failure mode.
- maybe we should have a button for putting all your veo into a channel.

* the light node should probably have more feedback when you do stuff.
- when you make a channel, it should say something about if the tx succeeded.

* make sure there is no epmd vulnerability letting people connect to our full nodes remotely.
 - Maybe setting the environment variable export ERL_EPMD_ADDRESS=127.0.0.1 will disable access from outside. I could test this at a later time

* test out the full contract resolution in light nodes to make sure we are displaying amounts correctly.x

* when sharing blocks, compress them first. we probably need a new http handler for this, since the existing handlers are assuming JSON format.

* enable running multiple instances of amoveo on the same machine using different ports.

* dominant assurance markets.

* light wallet improvements suggested by OK.

* combinatorial markets.

* spend_tx is using global state "mode". This should instead be passed to the function.

* maybe we should have a game to see who can keep testnet mining pools active most, during the game we encourage spamming each other and making unusual transactions to cause problems.

* consider adding a debugger. add this line to amoveo_core.app.src: `debugger, wx, reltool, erts, observer, tools, sasl, compiler, et, runtime_tools`
u simply activate them whenever you want via the shell:
```
debugger:start().
 observer:start().
 ```

* maybe `error_logger_hwm, 50` should be raised to 10 000.

* consider making a light node in python.

* look at the pull request for the escrow tool.

* get rock-paper-scissors working in chalang.

* teach the light node to generate messages about oracles. to make it easier to know when to vote.

* make sure that in the markets, evidence outcome always has a bigger nonce than no_publish.

* maybe tx_pool_feeder should make a new thread for each tx being added, and listen for a response. If it doesn't respond in time, then drop the tx.
- Maybe txs should return error codes instead of crashing

* improve signal|noise ratio in logging.

record tx_pool should keep track of the block hash that it is building on.

* we should have more rules for ignoring bad peers. If they send the same request too often, or if they send invalid data more than 10 times per minute. 

* during DDOS, sometimes nodes end up dropping all their peers, and are then unable to sync. We should refuse to black list the hard coded peers.
- dangerous, someone else might rent the same ip.


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

* an api for downloading blocks in a compressed format. good for blocks with smart contracts.
Probabilistic State Channels
==========


Why Amoveo needs this
=======

If we don't use the best tools available to us, then we will lose against a competitor who does.

Currently amoveo offers 2 ways to trade, and they have a trade-off.
p2p derivatives don't have a good market mechanism matching trades in batches. So you might not get as good a price.
the hub market mechanism requires locking up twice as much veo in channels, so it is too expensive for the hub to operate.

with probabilistic state channels, we can have a tool that solves both of these problems at the same time, with many other benefits besides.


scalability
======

Originally, the bitcoin scaled by the number of payments. The cost to the bitcoin system grew more or less linearly with the number of payments.

Channels meant that we could scale with the number of financial relationships.

channels + probabilistic payments meant that we could scale with the number of users.

probabilistic channels means we can scale with the number of channel hubs connecting the network together.

So for the same cost as making 1 payment per week on Bitcoin, you could create and maintain a channel hub on Amoveo and have tens of thousands of channel relationships, all with different turing complete smart contracts being updated every second.
It is a hugely more scalable design.

With our current level of technology, if we had dozens of competent programmers maintaining it, I think at best we couldn't sustain more than 10-100 million users, and for many users, Amoveo would only be creating a little more value than the cost of using it.
With probabilistic payments, we could sustain tens of trillions of accounts. The vast majority of users would never write anything on-chain or pay any miner fee.
The cost of using Amoveo would be essentially zero, so there are only benefits to using it.



liquidity in off-chain smart contracts
=========

A major limitation of channels is that they are terrible for lottery.
You can only win as much money as is in the channel.
prob-channels don't have this problem.
If you lock more than $1000 in stake, you could make 1000 channels, each with a 0.1% chance of winning $1000.

Similarly, if you are running a market powered by probabilistic state channel smart contracts, you could sell many mutually exclusive channel contracts using the same staked funds.
The only channels which can ever get created on-chain are the ones where all the other channels cannot get created.


currently if you want to run a market like amoveobook to match trades, you need have twice as much money locked in channels vs the amount actually at stake in the bet. Half the money is being canceled out by arbitrage.

These lockup costs mean that only very rich people can run a hub. Because 1/2 the money in a market at any time is money owned by the hub.
if we go with probabilistic channels instead, then the hub only needs to control something like 2% - 10% of the money in his markets.
Which means it costs a lot less to run a hub and launch some new markets to let people trade.


How a channel hub works
======

the hub keeps track of a merkel tree containing all the active prob-channel relationships. The merkel root of this is published in every block.

Multiple hubs work together, they put their merkel roots into a merkel tree, so even less data needs to be recorded on-chain.

The merkel tree stores each prob-channel in a location determined by the part of the probability space which results in this prob-channel winning the lottery. That way, a merkel proof of the existence of your prob-channel is also a proof that no one else has the same part of the probability space as you.

So if you create a prob-channel with the hub, the hub gives you a contract for rights over a certain part of the probability space, and he gives you a proof that this portion of the probability space in the merkel tree was recently empty.


the leverage of channel hubs
============

If a person keeps making prob-accounts, and then selling all the value in them for prob-channels, and then using his profits from these sales to make new prob-accounts to sell more prob-channels.
It is like he is getting a leveraged position.
The total value of all the prob-accounts he controls becomes much larger than the total value of the account he had started with.

So a person with only 1 veo can generate and profit from 20+ veo worth of prob-channels all containing smart contracts.

The capital cost of being a channel hub are very low. So it is cheap to launch a new channel hub and offer custom markets in whatever you care about.


off-chain channel hubs
===========

When a hub settles, and someone wins the lottery, if that account is different from the account that originally created the hub, then they should get paid with the creation of a new prob-account.

This way, if they were running an off-hub hub with off-hub prob-channels, it becomes a normal hub with normal prob-channels.

This gives some major advantages.

You can establish a hub without having to record any information on-chain. Even if your address isn't recorded on-chain anywhere, you can run a market that matches derivatives in single price batches, and you can recover liquidity from groups of prob-channels betting on mutually exclusive outcomes.

It means we can create off-hub channels without having to contact the hub.

Every time you update a prob-channel, you need to store about 250 additional bytes until the prob-channel can close.
By layering hubs inside of each other, any individual hub wont have to keep track of too many prob-channel updates. So the memory requirement of running a hub can be bounded.

By layering hubs, each individual hub can store less value.
So if you are running many different hubs, you can use a different private key for each one, so if one of your servers is compromised, you don't lose everything.

Parallelizing tx processing across multiple hubs increases throughput of txs.


channel hub gossip
========

The hub maintains a merkel tree of the prob-channels, and he regularly posts the merkel root onto the blockchain, along with a merkel root of a tree containing all the updates that happened between this on-chain post and the previous.

So a person interested in using a hub could sync all the hub-blocks of updates, and verify the merkel proofs, and know that the hub never double-spent anything. And the person syncing these hub-blocks doesn't have to store any merkel tree database on-chain, because every block comes with all the merkel proofs for all the data you need to verify that hub-block.

The hub blocks have 3 kinds of txs.
creating a prob-channel
updating the merkel root of all the prob-channel which are supported by using this prob-channel as a hub, the prob-channel signs.
closing a prob-channel, the prob-channel signs.

So by merely sycing the hub-blocks through a gossip protocol, we can know that a hub is honest, so we know whether we can use that hub.

Anyone who owns a prob-channel can use it as a hub, and start gossiping updates to peers.


tx types
====

1) probabilistic-deposit

* amount of money
* pubkey to control spending

2) probabilistic-withdraw

* a chalang spk signed by the owner which, if unlocked, enables this withdraw.
* a chalang script-sig to provide evidence to unlock the spk.
* if chalang_vm(script-sig ++ scipt-pubkey) returns true, then it is valid.

3) probabilistic-cancel

If someone tries doing a probabilistic-withdraw with expired data, this is how the anyone can evidence to prevent those bad withdraws.

4) probabilistic-channel

* you have to wait a long enough delay after the probabilistic-withdraw before you can do this tx.
* if there is more than one active valid probabilistic-withdraw for the same probabilistic-account, then the valid one is whichever had a merkel proof in a block first. Delete the creator's deposit, because he cheated.
* If the winner is different from who created the probabilistic-deposit, then this creates a new probabilistic-deposit that the winner controls.
* The new deposit has 80% of the money from the old one. 20% of the money goes back to whoever made the original prob-deposit supporting this hub.
* 10%/20% are just an example. It should work with 1%/2% as well. We will make this a variable, so the person running the channel hub can decide for themselves how big the incentive needs to be.

5) proof of existence

* This allows the creator to publish 32 bytes of data into the proof of existence tree. It keeps a record of the block height at which this hash was recorded.

<!---

prob-channel double-spend protection txs
===========

6) prob-channel challenge

* If the server has double-spent part of the probability space, this is how you publish a proof to punish them for doing this.

7) prob-channel response

* if someone made a prob-channel challenge, and the hub has not cheated, then that means the hub should have evidence that one of the conflicting prob-channels was closed or updated, by either providing a signed agreement to close the channel, or some different data to provide to one of the channels to make it close at a higher-nonced state.

8) prob-channel timeout

* if the hub failed to make prob-channel response in time, then eventually it becomes possible to make this tx type.
* most of the prob-account deposit gets deleted, but some goes to whoever made the prob-channel challenge.

Data availability txs
========

9) prob-channel data request

* if a hub is refusing to give you a merkel proof of your prob-channel state, this tx can be used to force that hub to either give you the proof, or else all the value in their hub is destroyed.
* before you can generate this tx for height H, you need a merkel proof that the hub committed to the state of the prob-channels at that height.

10) prob-channel data response

* this is how the hub can report on-chain to any data requests for merkel proofs.

11) prob-channel data request slash

* if the hub fails to do a prob-channel data response tx within the time limit, then it eventually becomes possible to do this tx.
* this deletes the channel-hubs deposit, and gives a small reward to whoever made the data request.

--->

New merkel tree data structures in the consensus state
============

1) lotteries

2) proof of existence

3) prob-channel challenges



Data the hub needs to store
=============

1) for every live prob-channel, he needs to store the channel state signed by the user, and a merkel proof of that prob-channel such that it is stored based on the part of the probability space that results in this prob-channel winning the lottery.
```32 * log16(#of live prob-channels)*(# of prob-channel updates) bytes```
So if there are 1000 users, and about 1 trade happens per second, and the smart contract lasts 2 months, then this will take up 32*log16(1000)*5000000bytes = about 400 megabytes.
If we limit the txs so you can only request data in the most recent week, we can get this down to 50 megabytes.

2) for every prob-channel that has existed, he needs to store a signed message where the user has agreed that the old version of the prob-channel is invalid. The signed message has a commit-reveal. He also needs to have the secret which was revealed for this commit-reveal.
signature + contract hash + commit + reveal
```(about 250 bytes) * (# of prob-channels)```
If there are 10k users over the cource of the 2 month period, we are looking at 2.5 megabytes of data.

3) a copy of every live smart contract with every customer, the customer's signature over the contract.
Generally a market's contracts are repetitive, so each can be comopressed to 100 bytes or so. The signature is like 150 bytes.
So if there are 1000 live prob-channels, this database takes up around 250 kilobytes.


Data the users need to store
==============

1) for your prob-channel, you need to keep a copy of the most recent prob-channel state signed by the hub. So 150 bytes of signature, plus however long your smart contract is.

2) you need the keep a merkel proof showing that your prob-channel exists. You can use this proof to punish the hub if they try to double-spend your money.
256*(log16(number of channel hubs) + log16(number of prob-channels in your hub)) = about 1280 bytes.


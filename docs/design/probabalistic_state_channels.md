Probabilistic State Channels
==========


scalability
======

Originally, the bitcoin scaled by the number of payments. The cost to the bitcoin system grew more or less linearly with the number of payments.

Channels meant that we could scale with the number of financial relationships.

channels + probabilistic payments meant that we could scale with the number of users.

probabilistic channels means we can scale with the number of channel hubs connecting the network together.

So for the same cost as making 1 payment per week on Bitcoin, you could create and maintain a channel hub on Amoveo and have tens of thousands of channel relationships, all with different turing complete smart contracts being updated every second.
It is a hugely more scalable design.




liquidity in off-chain smart contracts
=========

A major limitation of channels is that they are terrible for lottery.
You can only win as much money as is in the channel.
prob-channels don't have this problem.
If you lock more than $1000 in stake, you could make 1000 channels, each with a 0.1% chance of winning $1000.

Similarly, if you are running a market powered by probabilistic state channel smart contracts, you could sell many mutually exclusive channel contracts using the same staked funds.
The only channels which can ever get created on-chain are the ones where all the other channels cannot get created.


How a channel hub works
======

the hub keeps track of a merkel tree containing all the active prob-channel relationships. The merkel root of this is published in every block.

Multiple hubs work together, they put their merkel roots into a merkel tree, so even less data needs to be recorded on-chain.

The merkel tree stores each prob-channel in a location determined by the part of the probability space which results in this prob-channel winning the lottery. That way, a merkel proof of the existence of your prob-channel is also a proof that no one else has the same part of the probability space as you.

So if you create a prob-channel with the hub, the hub gives you a contract for rights over a certain part of the probability space, and he gives you a proof that this portion of the probability space in the merkel tree was recently empty.


Why closing a prob-channel creates a channel?
============

the idea of having 2 layers of turing completeness.

If we want to make updates to the first layer, that would require storing a signature, the hash of an spk, a 32 byte commitment, and a 32 byte reveal, for every time you update any channel. That is like 250 bytes per update.
So 1 gigabyte would allow for around 4 million smart contract updates.

But if you make a prob-channel that eventually turns into a channel on-chain, then we can update that second layer as many times as we want without increasing the storage requirement.

the on-chain channel ends up having the same amount of veo in it no matter how much veo is in your prob-channel. So we can update the prob-channel part of the smart contract completely independently of updating the channel part.

So we need these two layers of turing completeness, and smart contract designers need to split up their smart contract code into the two layers carefully to take full advantage of the scalability of Amoveo.

You need to use the prob-channel layer for your smart contract if you are selling multiple mutually exclusive versions of the same contract, and you want to take advantage of reusing the same liquidity in all the different prob-channels.

You might prefer putting your code in the channel layer if it involves making many millions of updates, and you can't afford to store all those 250 byte proofs that a prob-channel has been updated.


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

If a prob-channel has already been closed, and someone tried doing a probabilistic-withdraw for it anyway, this is how the channel hub provides evidence to prevent those bad probabilistic-withdraws.

4) probabilistic-channel

* you have to wait a long enough delay after the probabilistic-withdraw before you can do this tx.
* if there is more than one active valid probabilistic-withdraw for the same probabilistic-account, then delete 90% of the money, and give the rest to whoever published the 2nd probabilistic-withdraw, otherwise continue.
* this creates a normal on-chain channel, just like a new-channel-tx would. Once created, the spk that controls the funds in this channel is not necessarily at all related to the spk from the probabilistic-withdraw step.
* the channel has 80% of the money from the prob-account, the last 20% goes back to whoever made the prob-deposit initially.

5) proof of existence

* This allows the creator to publish 32 bytes of data into the proof of existence tree. It keeps a record of the block height at which this hash was recorded.

6) prob-channel challenge

* If the server has double-spent part of the probability space, this is how you publish a proof to punish them for doing this.

7) prob-channel response

* if someone made a prob-channel challenge, and the hub has not cheated, then that means the hub should have evidence of the first prob-channel being closed before the second prob-channel was opened.

8) prob-channel timeout

* if the hub failed to make prob-channel response in time, then eventually it becomes possible to make this tx type.
* most of the prob-account money gets deleted, but some goes to whoever made the prob-channel challenge.

9) prob-channel data request

* if a hub is refusing to give you a merkel proof of your prob-channel state, this tx can be used to force that hub to either give you the proof, or else all the value in their hub is destroyed.
* before you can generate this tx for height H, you need a merkel proof that the hub committed to the state of the prob-channels at that height.

10) prob-channel data response

* this is how the hub can report on-chain to any data requests for merkel proofs.

11) prob-channel data request slash

* if the hub fails to do a prob-channel data response tx within the time limit, then it eventually becomes possible to do this tx.
* this deletes 90% of the money in a prob-account, and gives the last 10% to whoever made the prob-channel data request.


New merkel tree data structures in the consensus state
============

1) prob-accounts

2) proof of existence

3) prob-channel challenges



Data the hub needs to store
=============

1) for every live prob-channel, he needs to store the channel state signed by the user, and a merkel proof of that prob-channel such that it is stored based on the part of the probability space that results in this prob-channel winning the lottery.
```32 * log16(#of live prob-channels)*(# of prob-channel updates) bytes```
So if there are 1000 users, and about 1 trade happens per second, and the smart contract lasts 2 months, then this will take up 32*log16(1000)*5000000bytes = about 400 megabytes.

2) for every prob-channel update, he needs to store a signed message where the user has agreed that the old version of the prob-channel is invalid. The signed message has a commit-reveal. He also needs to have the secret which was revealed for this commit-reveal.
signature + contract hash + commit + reveal
```(about 250 bytes) * (# of prob-channel updates)```
2 months with 1 trade per second is about 5 million updates. So this database would be about 1.25 gigabytes, in the example.

3) a copy of every live smart contract with every customer, the customer's signature over the contract.
Generally a market's contracts are repetitive, so each can be comopressed to 100 bytes or so. The signature is like 150 bytes.
So if there are 1000 live prob-channels, this database takes up around 250 kilobytes.


Data the users need to store
==============

1) for your prob-channel, you need to keep a copy of the most recent prob-channel state signed by the hub. So 150 bytes of signature, plus however long your smart contract is.

2) you need the keep a merkel proof showing that your prob-channel exists. You can use this proof to punish the hub if they try to double-spend your money.
256*(log16(number of channel hubs) + log16(number of prob-channels in your hub)) = about 1280 bytes.


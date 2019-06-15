Sortition Chains
=========

A sortition contract is a kind of smart contract.
If you are participating in a sortition contract, then either you will get all the money in the contract, or none of it.

Example:
You lock $10 into a sortition contract to bet at 50:50 odds on the outcome of a football game. At the end, you have $20 in the contract.
But the total value of the sortition chain is $1000. Since you have $20, what that means is that you have a 2% chance of winning the entire sortition chain of $1000.
Typically, after the end of the football game you would sell your stake in the sortition contract for $20, instead of holding such a high-risk asset.

A sortition contract can only exist as a part of a sortition chain.

A person usually doesn't want to hold a contract that only has a 2% chance of having value. That is a lot of risk. But as long as there are other people willing to buy the contract at a good price, this works.


Why Amoveo needs this
=======

If we don't use the best tools available to us, then we will lose against a competitor who does.

Currently amoveo offers 2 ways to trade, and they have a trade-off.
p2p derivatives don't have a good market mechanism matching trades in batches. So you might not get as good a price.
the hub market mechanism requires locking up twice as much veo in channels, so it is too expensive for the hub to operate.

with sortition chains, we can have a tool that solves both of these problems at the same time, with many other benefits besides.


scalability
======

Originally, the bitcoin scaled by the number of payments. The cost to the bitcoin system grew more or less linearly with the number of payments.

Channels meant that we could scale with the number of financial relationships.

With lightning channels we could scale with the number of users

Using sortition chains, the blockchain scales infinitely.

So for a lower cost than making 1 payment per week on Bitcoin, you could create and maintain a sortition chain on Amoveo and have tens of thousands of sortition contract relationships, all with different turing complete smart contracts being updated every second.
It is a hugely more scalable design.

With only state-channel technology, if we had dozens of competent programmers maintaining it, I think at best we couldn't sustain more than 10-100 million users, and for many users, Amoveo would only be creating only a little more value than the cost of using it.
At >10 million users, the operating costs would be high, it would start having centralizing effects.

With sortition chains, we could sustain hundreds of trillions of accounts for almost no on-chain cost. The vast majority of users would never write anything on-chain or pay any miner fee.
The cost of using Amoveo would be essentially zero, so there are only benefits to using it.


liquidity in sortition chains
=========

A major limitation of channels is that they are terrible for lottery.
You can only win as much money as is in the channel.
sortition chains don't have this problem.
If you lock $1000 in stake, you could make 1000 sortition contracts, each with a 0.1% chance of winning $1000.

Similarly, if you are running a market inside a sortition chain, you could sell many mutually exclusive sortition contracts using the same staked funds.

currently if you want to run a market like amoveobook to match trades, you need have twice as much money locked in channels vs the amount actually at stake in the bet. Half the money is being canceled out by arbitrage.

These lockup costs mean that only very rich people can run a hub. Because 1/2 the money in a market at any time is money owned by the hub.
if we go with sortition chains instead, then the market operator only needs to control something like 2% - 10% of the money in his markets.
Which means it costs a lot less to launch some new markets to let people trade.


How a sortition chain works
======

the sortition chain operator keeps track of a merkel tree containing all the active sortition contracts. The merkel root of this is published in every block.

The merkel tree stores each sortition contract in a location determined by the part of the probability space which results in this contract winning the lottery. That way, a merkel proof of the existence of your sortition contract is also a proof that no one else has the same part of the probability space as you.

So when you create a sortition contract with a sortition chain operator, the operator is giving your a contract for rights over a certain part of the probability space, and he gives you a proof that this portion of the probability space was recently empty.


the leverage of sortition chains
============

If a sortition chain operator keeps selling sortition contracts, eventually they will make back almost all the money that they had paid to create the soritition chain, which means they have enough money to make another sortition chain.
It is like he is getting a leveraged position.
The total value of all the sortition chains he is operating becomes much larger than the total value of the account he had started with.

So a person with only 1 veo can generate and profit from 20+ veo worth of sortition chains all containing smart contracts.

The capital cost of being a sortition chain operator is very low. So it is cheap to launch a new sortition chain and offer custom markets in whatever you care about.


Sortition contracts are sortition chains
===========

We sometimes call them "contracts" and sometimes call them "chains" to hint at how it is being used in the current context. If you are using it to make a bet, we call it a contract. If you are using it to run a market where other people can bet, we call it a chain.

If we are talking about the children of a sortition chain, we will often defualt to calling them "contracts".

When a sortition chain settles, and someone's sortition contract wins the lottery, if that account is different from the account that originally created the sortition chain, then they should get paid with the creation of a new sortition chain on-chain.

What this means is that any sortition contract owned by someone besides the sortition chain operator, that sortition contract is a sortition chain, and the owner is the sortition chain operator of that chain.

But before you start selling contracts in your sortition chain, you will need to set up a server with a database for storing all the sortition contracts, and to automatically post the merkel roots to either the blockchain, or to one of your ancestor's sortition chains.

"contracts == chains" scaling advantages
==================

If you are running a sortition chain on top of a sortition chain, you have the option of storing your merkel roots inside the parent merkel tree, instead of storing it on the main chain. You can also store in a grandparent, or any other ancestor.

So there are around 1000 sortition chains being refered to by the main blockchain, and each of these chains is supporting 1000+ more sortition chains, and each of those could be supporting 1000+ sortition chains, and each of those could be supporting 1000+ sortition contracts.
So we can support over 1 trillion smart contracts, even with only 1000 small structures being recorded on-chain.

"contracts == chains" UX advantages
==========

If you are making a new sortition on top of an existing one, it isn't going to be recorded on-chain immediately. You don't have to wait for any confirmations or pay any tx fees. Your address isn't even recorded on-chain anywhere. And you can run a market that matches derivatives in single price batches.

"contracts == chains" resource consumption advantages
===================

By layering sortition chains inside of each other, any individual sortition chain wont have to keep track of too much data. So the memory requirement of running a sortition chain can be bounded.

By layering sortition chains, each individual sortition chain can store less value.
So if you are running many different sortition chains, you can use a different private key for each one, so if one of your servers is compromised, you don't lose everything.

Parallelizing tx processing across multiple computers running different sortition chain databases increases throughput of txs.


gossip, the "chain" part of "sortition chain"
========

The sortition chain maintains a merkel tree of the sortition contracts, and it regularly posts the merkel root onto the blockchain, along with a merkel root of a tree containing all the updates that happened between this on-chain post and the previous.

So a person interested in using a sortition chain could sync all the sortition-blocks of updates, and verify the merkel proofs, and know that the sortition chain has never double-spent anything. And the person syncing these sortition-blocks doesn't have to store any merkel tree database, because every block comes with all the merkel proofs for all the data you need to verify that sortition-block.

The sortition blocks have 3 kinds of txs.
* creating a sortition contract.
* updating the merkel root of all the sortition contracts which are supported by the child sortition chain.
* closing a sortition contract.

So by merely sycing the sortition-blocks through a gossip protocol, we can know that a sortition chain is honest, so we know whether we can use that sortition chain.
You have to sync all the blocks of the sortition chain you care about, and it's parent, and the parent-parent, all the way back to the main chain.


tx types
====

1) sortition new

* almost identical to new_channel_tx
* pubkey to control spending
* amount of money
* expiration date for when it it becomes possible to make sortition-contract-txs for this sortition chain.

2) sortition contract

* almost identical to channel_solo_close_tx
* a chalang spk signed by the owner which, if unlocked, enables this withdraw.
* a chalang script-sig to provide evidence to unlock the spk.
* if chalang_vm(script-sig ++ scipt-pubkey) returns true, then it is valid.

3) sortition slasher

* almost identical to channel_slash_tx

If someone tries doing a sortition-contract with expired data, this is how anyone can provide evidence to prevent those bad withdraws.
You provide a merkel proof to the signed sortition-tx where either the sortition contract was either closed, or updated.
If this sortition chain used to be the child of another sortition chain, you may need merkel proofs of the ancestor's state in order to get the merkel root of your own history, then when you have your own merkel root you can verify a merkel proof of your own history.

4) sortition timeout

* almost identical to channel_timeout_tx
* you have to wait a long enough delay after the sortition-contract-tx before you can do this tx.
* if there is more than one active valid sortition-contract for the same sortition-chain, then the valid one is whichever had a merkel proof in a block first. Delete the creator's deposit, because he cheated.
* If the winner is different from the sortition chain operator, then this creates a new sortition chain that the winner controls.
* The new sortition chain has 80% of the money from the old one. 20% of the money goes back to the operator of the now closed sortition chain, as a safety deposit that was influencing them to act responsibly.
* 10%/20% are just an example. It should work with 1%/2% as well. We will make this a variable, so the person running the sortition chain can decide for themselves how big the incentive needs to be.
* the new sortition chain has an expiration that is already passed. So it is possible to start the process of settling this sortition chain immediately.

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

1) sortition chains

* pubkey for spending
* amount of veo
* expiration date


2) proof of existence

* arbitrary 32-bytes.
* the height where this was recorded.

3) sortition contract results (generated from the sortion-contract-tx and deleted by sortition-slash-tx)

* pubkey who will receive the veo
* sortition id
* contract nonce

if we can generate a higher nonce for the same sortition id, then that means this contract is invalid.


Data the sortition chain needs to store
=============

1) all the sortition-blocks with merkel proofs.
each tx is about 1 kb.
1 trade per second for 2 months would be like 5 gigabytes.


2) for each user, merkel proof of that sortition contract such that it is stored based on the part of the probability space that results in this sortition contract winning the lottery.
```32 * log16(#of live sortition contracts)*(# of live sortition contracts) bytes```
So if there are 1000 users, and about 1 trade happens per second, and the sortition chain lasts 2 months, then this will take up 32*log16(1000)*5000bytes = about 400 kilabytes.

3) for every sortition contract that has existed in his sortition chain, he needs to store a signed message where the user has agreed that the old version of the sortition contract is invalid. The signed message has a commit-reveal. He also needs to have the secret which was revealed for this commit-reveal.

signature + contract hash + commit + reveal
```(about 250 bytes) * (# of sortition contracts)```
If there are 10k users over the cource of the 2 month period, we are looking at 2.5 megabytes of data.

4) a copy of every live smart contract with every customer, the customer's signature over the contract.
Generally a market's contracts are repetitive, so each can be comopressed to 100 bytes or so. The signature is like 150 bytes.
So if there are 1000 live sortition contracts, this database takes up around 250 kilobytes.


Data the users need to store
==============

1) for your sortition contract, you need to keep a copy of the most recent contract state signed by the sortition chain operator. So 150 bytes of signature, plus however long your smart contract is.

2) you need the keep a merkel proof showing that your sortition contract exists. You can use this proof to punish the sortition chain operator if they try to double-spend your money.
256*(log16(number of sortition contracts in your sortition chain)) = about 1280 bytes.

3) you need to download all the sortition-blocks, but you don't need to store them.
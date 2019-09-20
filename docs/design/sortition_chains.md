Sortition Chains
=========

This is a scaling plan, similar to sharding or side-chains.

Related documents:

[sortition chain attacks analyzed](./sortition_chains_defense.md)

[sortition chain implementation details.](./sortitoin_chains_implementation.md)

[sortition chain theory.](./sortition_chains_theory.md)


What is a Sortition Chain?
=========

A sortition contract is a kind of smart contract.
If you are participating in a sortition contract, then either you will get all the money in the contract, or none of it.
It is always possible to divide a sortition contract into two lower valued contracts, who would both win value in mutually exclusive situations. And it is always the case that expected_value(big) = expected_value(little_1) + expected_value(little_2)

Example:
You lock $10 into a sortition contract to bet at 50:50 odds on the outcome of a football game. At the end, you have $20 in the contract.
But the total value of the sortition chain is $1000. Since you have $20, what that means is that you have a 2% chance of winning the entire sortition chain of $1000.
Typically, after the end of the football game you would sell your stake in the sortition contract for $20, instead of holding such a high-risk asset.

A sortition contract can only exist as a part of a sortition chain.

A person usually doesn't want to hold a contract that only has a 2% chance of having value. That is a lot of risk. But as long as there are other people willing to buy the contract at a good price, this works.

Updating an existing sortition contract between 2 people can happen instantly, and can be a part of a lightning payment.

Why Amoveo Needs This
=======

If we don't use the best tools available to us, then we will lose against a competitor who does.

Currently amoveo offers 2 ways to trade, and they have a trade-off.
p2p derivatives don't have a good market mechanism matching trades in batches. So you might not get as good a price.
the hub market mechanism requires locking up twice as much veo in channels, so it is too expensive for the hub to operate.

One of the many benefits of using sortition chains is the ability to access a tool that solves both of these problems simultaneously.


Scalability
======

Originally, the bitcoin scaled by the number of payments. The cost to the bitcoin system grew more or less linearly with the number of payments.

Channels meant that we could scale with the number of financial relationships.

With lightning channels we could scale with the number of users

Using sortition chains, the blockchain scales infinitely.

So for a lower cost than making 1 payment per week on Bitcoin, you could create and maintain a sortition chain on Amoveo and have tens of thousands of sortition contract relationships, all with different turing complete smart contracts being updated every second.
It is a hugely more scalable design.

With only state-channel technology, if we had dozens of competent programmers maintaining it at best we could not sustain more than 10-100 million users. For many users, Amoveo would only be creating slightly more value than the cost of using it.
At >10 million users, the operating costs would be high and would begin having centralizing effects.

With sortition chains, we could sustain hundreds of trillions of accounts for almost no on-chain cost. The vast majority of users would never write anything on-chain or pay any miner fee.
The cost of using Amoveo would be essentially zero, thus there are only benefits to using it.


Liquidity in Sortition Chains
=========

A major limitation of channels is that they are terrible for lottery.
You can only win as much money as is in the channel.
Sortition chains don't have this problem.
If you lock $1000 in stake, you could make 1000 sortition contracts, each with a 0.1% chance of winning $1000.

Similarly, if you are running a market inside a sortition chain, you could sell many mutually exclusive sortition contracts using the same staked funds.

If you want to run a market like amoveobook to match trades, you need to have twice as much money locked in channels vs the amount actually at stake in the bet. Half the money is being canceled out by arbitrage.

These lockup costs mean that only very rich people can run a hub (since 1/2 the money in a market at any time is money owned by the hub).

If we go with sortition chains instead, then the market operator only needs to control something like 2% - 10% of the money in his markets (whiich means it costs a lot less to launch new markets for people to trade).


Sortition Chain Operators
======

The sortition chain operator keeps track of a merkel tree containing all the active sortition contracts. The merkel root of this is published in every block.

Every branch of the merkel tree has bits of chalang code, which specify the mutually exclusive conditions which could allow each sub-branch to be active.
The chalang code can reference randomness, or the output of an oracle, or anything.
That way, a merkel proof of the existence of your sortition contract is also a proof that no one else has the same part of the probability space as you.

So when you create a sortition contract with a sortition chain operator, the operator is giving you a contract for rights over a certain part of the probability space, and he gives you a proof that this portion of the probability space is owned by him.


The Leverage of Sortition Chains
============

If a sortition chain operator keeps selling sortition contracts, eventually they will make back almost all the money that they had paid to create the soritition chain, which means they have enough money to make another sortition chain.
It is like he is getting a leveraged position.
The total value of all the sortition chains he is operating becomes much larger than the total value of the account he had started with.

So a person with only 1 veo can generate and profit from 20+ veo worth of sortition chains all containing smart contracts.

The capital cost of being a sortition chain operator is very low. So it is cheap to launch a new sortition chain and offer custom markets in whatever you care about.


Sortition Contracts are Sortition Chains
===========

We sometimes call them "contracts" and sometimes call them "chains" to hint at how it is being used in the current context. If you are using it to make a bet, we call it a contract. If you are using it to run a market where other people can bet, we call it a chain.

If we are talking about the children of a sortition chain, we will often defualt to calling them "contracts".

When a sortition chain settles, and someone's sortition contract wins the lottery, if that account is different from the account that originally created the sortition chain, then they should get paid with the creation of a new sortition chain on-chain.

What this means is that any sortition contract owned by someone besides the sortition chain operator, that sortition contract is a sortition chain, and the owner is the sortition chain operator of that chain.

But before you start selling contracts in your sortition chain, you will need to set up a server with a database for storing all the sortition contracts, and to automatically post the merkel roots to either the blockchain, or to one of your ancestor's sortition chains.

"contracts == chains" Scaling Advantages
==================

If you are running a sortition chain on top of a sortition chain, you have the option of storing your merkel roots inside the parent merkel tree, instead of storing it on the main chain. You can also store in a grandparent, or any other ancestor.

So there are around 1000 sortition chains being refered to by the main blockchain, and each of these chains is supporting 1000+ more sortition chains, and each of those could be supporting 1000+ sortition chains, and each of those could be supporting 1000+ sortition contracts.
So we can support over 1 trillion smart contracts, even with only 1000 small structures being recorded on-chain.

"contracts == chains" UX Advantages
==========

If you are making a new sortition on top of an existing one, it isn't going to be recorded on-chain immediately. You don't have to wait for any confirmations or pay any tx fees. Your address isn't even recorded on-chain anywhere. And you can run a market that matches derivatives in single price batches.

"contracts == chains" Resource Consumption Advantages
===================

By layering sortition chains inside of each other, any individual sortition chain wont have to keep track of too much data. So the memory requirement of running a sortition chain can be bounded.

By layering sortition chains, each individual sortition chain can store less value.
So if you are running many different sortition chains, you can use a different private key for each one, so if one of your servers is compromised, you don't lose everything.

Parallelizing tx processing across multiple computers running different sortition chain databases increases throughput of txs.

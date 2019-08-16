Sortition Chains
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


How a Sortition Chain Works
======

The sortition chain operator keeps track of a merkel tree containing all the active sortition contracts. The merkel root of this is published in every block.

Every branch of the merkel tree has bits of chalang code, which specify the mutually exclusive conditions which could allow each sub-branch to be active.
The chalang code can reference randomness, or the output of an oracle, or anything.
That way, a merkel proof of the existence of your sortition contract is also a proof that no one else has the same part of the probability space as you.

So when you create a sortition contract with a sortition chain operator, the operator is giving you a contract for rights over a certain part of the probability space, and he gives you a proof that this portion of the probability space was recently empty.


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


Sortition Blocks: The Chain in 'Sortition Chain'
========

The sortition chain maintains a merkel tree of the sortition contracts, and it regularly posts the merkel root onto the blockchain, along with a merkel root of a tree containing all the updates that happened between this on-chain post and the previous.

So a person interested in using a sortition chain could sync all the sortition-blocks of updates, and verify the merkel proofs, and know that the sortition chain has never double-spent anything. And the person syncing these sortition-blocks doesn't have to store any merkel tree database, because every block comes with all the merkel proofs for all the data you need to verify that sortition-block.

The sortition blocks have 3 kinds of txs.
* creating a sortition contract.
* updating the merkel root of all the sortition contracts which are supported by the child sortition chain.
* closing a sortition contract.
* make a probabilistic deposit.

So by merely syncing the sortition-blocks, we can know that a sortition chain is honest, so we know whether we can use that sortition chain.
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
* a list of tuples [{script-pubkey, script-sig, proof, signature}|...]
* a chalang spk signed by the owner which, if unlocked, enables this withdraw.
* a chalang script-sig to provide evidence to unlock the spk.
*  chalang_vm(script-sig ++ scipt-pubkey) must return true
* it creates a list like [{Nonce1, Heigh1}|...]
* it compares this nonce-height list against the last sortition-contract published to see if we should update.. like compare(New, Old).
* You pay a safety deposit.

3) sortition cancel

* very similar to channel_slash_tx
* if anyone ever signs some data saying that they want to close a sortition, and they try publishing a sortition-contract-tx with the expired contract, then you can use that data to make this tx.
* you win the safety deposit from the sortition-contract-tx.

4) sortition timeout

* almost identical to channel_timeout_tx
* if there are multiple ways the sortition could be closed, we use this rule to choose just one. We are comparing the nonce-height lists from each way the channel can be closed
```
compare([X|T1], [X|T2]) -> compare(T1, T2);
compare([{N1, _}|T1], [{N2, _}|T2]) ->
              N1 > N2;
compare([{_, H1}|T1], [{_, H2}|T2]) when (H1 < H2) ->
             H1 < H2;
compare([], []) -> false.
```

* you have to wait a long enough delay after the sortition-contract-tx before you can do this tx.
* If the winner is different from the sortition chain operator, then this creates a new sortition chain that the winner controls.
* The new sortition chain has all the money from th eold one.
* the new sortition chain has an expiration that is already passed. So it is possible to start the process of settling this sortition chain immediately.
* this unlocks the safety deposit you had paid in the sortition-contract-tx.

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

New Merkel Tree Data Structures in the Consensus State
============

1) sortition chains

* pubkey for spending
* amount of veo
* expiration date
* list of nonce-height pairs for sortitions in the process of being closed, along with the pubkey of who is in line to win.


2) proof of existence

* arbitrary 32-bytes.
* the height where this was recorded.

3) nonce-height lists for settling sortition chains

* each sortition chain can have 0 or more nonce-height lists in the process of settling it.
* looks like: `[{nonce1, heigh1}, {nonce2, height2}|...]`


Data the sortition chain operator needs to store
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


Data the Users Need to Store
==============

1) for your sortition contract, you need to keep a copy of the most recent contract state signed by the sortition chain operator. So 150 bytes of signature, plus however long your smart contract is.

2) you need the keep a merkel proof showing that your sortition contract exists. You can use this proof to punish the sortition chain operator if they try to double-spend your money.
256*(log16(number of sortition contracts in your sortition chain)) = about 1280 bytes.

3) you need to download all the sortition-blocks, but you don't need to store them.



What if a Sortition Chain Operator Sells All Their Stake, and then Goes Offline?
===========

Looking at the example of 3 generations of sortition chains, where the middle generation has sold all their stake, and then gone off-line.

Now the grandparent generation wants to buy back all the contracts it sold to hedge it's risk before the sortition chain ends, but the middle generation is gone, so it can't update the contracts with them.

The operators of the grandchildren chains can make sortition contracts with the operator of the grandparent chain, selling all of the contract back to the grandparent, without the middle generation even needing to come online.

So the operator of the grandparent sortition chain ends up controllig many many of his great-grandchild sortition chains.

For this to work, sortition chains need to have the option of reporting the merkel root of their sortition-contracts onto any ancestor, not just the direct ancestor. That way you can still update your contract, even if the person you bought it from went off-line.


Recovering Memory for Sortition Contracts that are Many Generations Away from the On-chain Sortition Chain.
=============


In VM language design, there are a few ways to implement functions.
In a language like C, every time a function gets called, you need to put a new pointer on the call stack. So if you have too many nested functions, eventually the call stack overflows and the program crashes.

In a language like erlang, we have tail-call optimizations. This means that if you format the function call according to some rules, then no memory is wasted when nesting functions inside of each other.


If our state-chain has too many layers of descendants, like 50+ layers, it could start getting very time-consuming to get your money out of the state-chain.
We have to keep shutting down one layer, which generates the next layer on-chain, and then shutting down that layer.

It would be nice if people could feel comfortable making sortition-contracts that are thousands of generations away from the on-chain version, because then we wouldn't need any sort of payment-hubs staying online.
Creating a new sortition contract as a descendant of your existing contract could be the standard way of making payments.

But this only makes sense if we have some way of collapsing the on-chain settlement procedure so it doesn't take thousands of blocks to settle a single sortition chain.
It seems like the sortition-contract-tx would have to accept a list of smart contracts instead of a single smart contract.
And instead of storing a single integer nonce, it stores a list of nonces.

So when we compare 2 alternative histories, we care about the first nonce that is higher in the list. so
[5, 1, 0, 0, 0] > [5, 0, 100, 100, 100]
So lets go back to that example where the operator of a sortition chain also controls most of the great-grandchildren sortition chains of that chain.

Lets say he won. So to prove that he won, he would need to publish the 4 smart contracts.

So lets add this missing ingredient: the owner of a sortition-contract should be able to generate valid sortition-txs to close that contract, even if the sortition tx doesn't get included in any merkel proof.

So that means he can publish the 4 smart contracts on-chain, along with a signed explanation saying that this sortition contract was closed.
Since that sortition contract was closed, it is possible for him to make a 1-smart contract sortition-contract-tx to get his money out instead of the 4-step version.

Next we add some safety deposits to disincentivize anyone from publishing a 4-smart contract version when a 1-smart contract version is available.

And that solves our objective. It collapses the amount of smart contracts that get published on-chain, the same way tail-call-optimization saves state in VM design for programming languages.

Using this trick, you can always promote your sortition contract from one sortition-chain into the level of one of it's ancestors, as long as the ancestor cooperates. The middle generations don't matter.

So we don't have to worry about whether the middle generations stay online.
If we buy a smart contract, we can still sell that smart contract even if the market where we bought it has gone offline.


Lightning Sortition Contract Creation
===========

a 2.2 secure way to do a lightning payment to create a new sortition contract.

But it can only work if
* you have an existing sortition contract capable of accepting as much value as you will control in the new sortition contraxt
* there is a lightning path between you and the person that you want to make the new sortition contract with.

The trick is that if you want a new sortition contract, then you just buy insurance against the possibility that it will not get created.
You use an existing lightning path to buy this insurance.

Once the sortition contract is created, then the insurance contract can never pay out, so it can be deleted.


The cost of buying insurance is far less than the volume of money that can be held in your sortition contract, because you can buy an empty contract.

The cost to the server is (6 block periods of confirmations)*(interest rate)*(amount of money which you will be able to receive I the new sortition contract)

So this is a way to instantly increase the amount of value you can potentially receive in sortition contracts more than 1000x, within a few confirmations, trustlessly.

But, you need an existing sortition contract before you can do this trick.

The value that you control in the sortition contract that is about to be created needs to be less than your total insurance coverage.
Your total insurance coverage needs to be less than the amount of veo you could possibly receive in existing sortition contracts.




Probabilistic Payments vs Sortition Chains
=============

creating a new sortition contract can be a replacement for probabilistic payments.
They can both be a part of lightning payments, so you don't have to wait for confirmations for either.




What if an attacker tries to increase the number of lotto tickets available, because they have more appatite for risk?
===========

If you stand to have a 5% expected gain every time you can buy lotto tickets, then you might be incentivized to start doing things that will increase the total number of lotto tickets available for you to purchase.

if things are efficient, then there would be many people buying lotto tickets for a small expected profit.
They would keep under-cutting each other, so there wouldn't be almost any incentive to increase the number of lotto tickets.

Purposefully increasing the number of lotto tickets is twice as capital intensive vs just buying up existing lotto tickets.
So they will keep under-cutting each other until the attack stops happening.



Using fraud proofs, maybe you don't actually have to sync any sortition blocks
========

This is a very speculative idea, I am not sure this is possible.

[influenced by Paul Sztorc's work on the subject](http://www.truthcoin.info/blog/fraud-proofs/)

Since sortition contracts are turing complete, it seems like it should be possible to have contracts that incentivize people to warn you if you are using an invalid sortition chain.



Mixing Merkel proofs with contracts
=========

We want the merkel proof to be a proof that the server has not double-spent your part of the probability space. So we need bits of software in every branch of the merkel tree.

A merkel proof looks like this.

```proof = [stem, stem, stem, leaf];
leaf = {address, contractID};
address - this is the address of the person who owns the sortition contract being proved.
stem = {chalang_bool, evidence, hash1, hash2} // if chalang_vm(evidence ++ chalang_bool) returns true, then hash1 needs to match the hash of the next element of the proof. if it returns false, then hash2 needs to match the next element of the proof.
// chalang_bool - this is some chalang code. It outputs 2 values. the first is a true/false for which branch of the merkel tree is being executed. 0 is false, any other value is true. The second value is the nonce for this version of the contract.
```

We need to keep track of gas while verifying merkel proofs, since the chalang_bool step is turing complete.

example chalang_bool contracts:

`int 7 int 13 rand_bit` This contract has a 7/13th chance of returning `true` and a 6/13th chance to return `false`

`OracleID oracle_lookup dup int 2 == if int 3 int 4 rand_bit else drop drop then` This contract returns `true` if the oracle is `true`. it returns `false` if the oracle is `false`, and if the oracle is `bad_question`, it has a 3/4ths chance of being `true` and a 1/4th chance of being `false`.



So lets make a very concrete example of a merkel proof.
Lets imagine Bob bought a contract worth $20, and the sortition chain has $1000 in it.

rand_int is a new function we need to add to chalang. it takes 3 integers as input, and uses the random seed embedded into the merkel proof verifier for entropy.

rand_int ( A B -- true/false )

It is required that A =< B.

It has an A / B probability of returning true, and (B-A) / B probability of returning False.


[{stem, [], compile("int 1 int 100 rand_int"), hash1, hash2},{stem, [], compile("int 1 int 2 rand_int"), hash3, hash4}, {stem, [], compile(" OracleID0 oracle_lookup dup int 2 == if int 1 int 2 rand_bool else drop drop then "), hash5, hash6}, {address, contractID]

where

```
hash1 = hash(compile("int 1 int 2 rand_int") ++ hash3 ++ hash4;
hash4 = hash(compile(" OracleID0 oracle_lookup dup int 2 == if int 1 int 2 rand_bool else drop drop then ") ++ hash5 ++ hash6;
hash5 = hash({address, contractID});
```

and given the random entropy programmed into this sortition chain, the "int 1 int 100 ran_int" contract needs to return true, and the oracle needs to either return true, or it's bad question version needs to randomly select true. and the "int 1 int 2 rand_int" contract needs to return false.

So this is a contract that has a 1/200th the value of the sortition chain it is a part of, and it is a winning bet in an oracle.


entropy for contracts
=============

If our merkel tree has multiple tiny contracts, it would be inefficient to re-load the block hash into the random number generator over and over for every contract. and it would be bad if we re-used the same random bits on multliple contracts, because that could cause correlation between the multiple layers, so some lottery tickets would be worth double what we had intended, and other lottery tickets will be worth nothing.

So I am thinking when we verify a merkel proof, we should first check that all the hashes match, then we should append all the short chalang contracts together with some glue code in between, so we can verify them all in one go. So we only run the VM once per merkel proof.

example tree update
============

`operator` is who made the merkel tree

tree starts empty, which means 100% chance it goes to the operator. `operator`

Bob buys 1% of the value`{"int 1 int 100 int 1 rand_bool int 1", bob, operator}`

Alice buys 2% of the value `{"int 1 int 100 int 1 rand_bool int 1", bob, {"int 2 int 100 int 1 rand_bool int 1", alice, operator}}`

Carol buys 1% of the value `{"int 1 int 50 int 1 rand_bool int 1", {"int 2 int 100 int 1 rand_bool int 1", alice, bob}, {"int 3 int 100 int 1 rand_bool int 1", carol, operator}}`

The key to look up your name in the merkel tree is any entropy value and oracle values such that you would win.
By using that as the key, the sortition chain operator is free to rewrite the contracts as necessary to keep everything in balance.

the rules for updating the tree need to be deterministically defined somewhere, that way anyone syncing the sortition-blocks will calculate the same root.


Contract Table
=========

Many contracts are appearing repeatedly. So we should keep a single copy of them in a different table, and refer to them by hash.
For example, if 4 people are betting on 4 mutually exclusive outcomes, the tree could look something like this:
```
{Contract1, {Contract2, Alice, Bob}, {Contract2, Carol, Dan}}
```
If contract1 and contract2 are both long, then it is best to avoid having to write one of them out twice in the database.


multiple random samples
============

Maybe a sortition chain would be better if we did multiple random samples instead of just 1.

So when a sortition chain closes, it could randomly pay to up to 100 accounts, each receiving 1% of the veo from that chain.

The trade-off is that the on-chain cost per sortition chain increases 100x, but the risk of holding lottery tickets reduces 10x.

We can already know that this strategy is no good, because costs are increasing by the square of the benefits.

Therefore it is always better to have 2 small sortition chains that each pay out once, instead of 1 big one that pays out twice.


lottery randomness is necessary for sharding
==============

properties we want for sharding

* cost of bandwidth to run a full node should at worst be as expensive as O(log(#users))
* the currency on different shards is fungible.
* the same consensus mechanism is securing all the shards at once.

The goal of this proof is to show that any blockchain with all 3 of these properties at once must be using some lottery randomness type accounts.

If we are using the same single consensus mechanism to secure all the shards, then that means there is a main chain that records the order of history of the heart of the consensus mechanism.

For the same consensus mechanism to secure all the shards, and have fungibility, that means it needs to be possible to move your money onto or through the main chain without anyone else's permission, just by giving txs to miners.

if the bandwidth is only O(log(#users)) or less, then that means the number of accounts recorded on the main chain needs to be less than O(log(#users)).

For it to be possible to move your value onto the main chain without anyone's permission, while it also being the case that only O(log(#users)) at most have their account recorded on the main chain, the only way to have both of these at once is if it is lottery-ticket type value.


Making the value less probabilistic
===============
There are ways to compromise to make the value less probabilistic.
Like if I had 10 sortition chains going at once, and everyone who make a contract in one, I did the identical contract in all 10 with them.

So now there are 10 winners instead of 1 winner.
Which significantly reduces the risk.

Each sub-sortition chain of the recursive tree can still have only 1 winner.
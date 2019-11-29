Optimistic Rollups sidechain attack
==========

[optimistic rollup review home](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups.md)

This is an attempt to show that if optimistic rollup scales worse than linearly.
If you increase the rate of tx production, the fee per tx necessarily increases.

A successful scaling solution would have the tx fees decrease as the total number of txs increases.

optimistic rollup has shards
===========

If we increase the amount of available data we can store in the consensus space, but still try to maintain only one single merkel tree of everyone's balances, we would run into scaling limitations.
It just isn't possible to insert elements into a single merkel tree and re-calculate the merkel roots above a certain rate. This computation has some aspects that are not parallelizeable.
So no matter how many computers we add to the network, there will still be an upper limit on how quickly we can update account balances in any single merkel tree.

So to overcome this limit, we would need to maintain multiple databases of account balances, called shards. And we need some slower mechanism to move value from one shard into another.

Optimistic rollup shards need leadership
==============

Lets suppose that more than one person has permission to publish a sidechain block. If that is the case, then it would be possible for any of them to do level 3 griefing attacks.

A sidechain block needs to hold many txs, but an attacker can build a contradictory sidechain block that only contains 1 tx, and he can get his small contradictory block inserted in front of the large block.

So the cost to the attacker is the cost of publishing a single tx into the available consensus state. And the destruction caused by the attack is proportional to the size of an average sidechain block, which we know from [fraud proof analysis](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups_fraud_proof_cost.md) must be at least O(sqrt(sqrt(total # of txs in the available consensus space))).
Since the destruction is bigger than the cost of the attack, this would be a level 3 attack. So that means it would not be secure to have a sidechain where more than one person has permission to publish blocks.

So that means that for any moment in time, there needs to be someone, or some group of people with exclusive permission to make sidechain blocks. They are the leadership for that sidechain.

Permanent leadership does not work
============

if the leadership for any one sidechain was permanent, then that sidechain would be vulnerable to soft fork bribery attacks. https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

As long as the leadership is being swapped out regularly, and you are unable to predict who the next leaders will be, and it is infeasible to bribe all the validators for all the shards all at once, then bribing is ineffective. Because you don't know who you would need to lock into a contract to have the guarantees you need to start executing the attack.

If the leadership is only swapped out very rarely, it can still be profitable to execute this attack for a single round of leaders. So we need it to be the case that the cost of rotating leadership is not too expensive.

You can earn some profit based on how long until they swap out, and how much value is in the shard. And the cost is based on how much stake could potentially get punished if they do this attack.

Leaders per sidechain
===========

if we want the profit of an attack to be constant, then `coins in a shard ~ stake per shard`

we know from analyzing fraud attacks that the number of shards is `sqrt(rate of tx production)`, so the portion of the coins in each shard must be `sqrt(rate of tx production)`

Assuming the stake per leader stays constant.

leaders per shard ~ sqrt(rate of tx production)

Cost of rotating leadership
===========

The cost of swapping leadership is the same as the cost of the leaders each having to sync another one of the sidechains.

cost of rotation = O((# leaders)*(cost to sync a sidechain))

The cost to sync a sidechain is at least as expensive as O(the size of that sidechain). We know from analyzing fraud proofs that the size is at least O(sqrt(rate of tx production)).


we know from analyzing fraud proofs that the number of sidechains is at least O(sqrt(rate of tx production)).



= `O((# shards)*(# leaders per shard)*(size of a sidechain))`


= `O(sqrt(rate of tx production)*(sqrt(rate of tx production))*(sqrt(rate of tx production)))`

cost of rotation = `O((rate of tx production)^(3/2))`

So this means the cost per tx in each sidechain would need to increase as the rate of tx production in the entire optimistic rollup network increases.
If the optimistic rollup network gets 4x bigger, the minimum tx fee in all the shards will get at least 2x bigger.




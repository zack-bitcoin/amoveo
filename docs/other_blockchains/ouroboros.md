Ouroboros Review
========

Ouroboros is a proof of stake type blockchain consensus mechanism. Here is the paper describing it https://eprint.iacr.org/2016/889.pdf

I have written a general paper about why [PoS is not possible](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md)

The goal of this paper is to analyze ouroboros to see if they have managed to prove me wrong. I will try to show that Ouroboros can not be secure.

[Here people attempt to show that the attack in this paper wont work](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/the_defence_of_pos.md)

Others have written about the myth that PoS is cheaper than PoW, so I will just [link to their work on the subject](http://www.truthcoin.info/blog/pow-cheapest/)


This vulnerability has been more or less understood since 2016 when I told Charles Hoskinson, the CEO of Cardano, about it on twitter https://twitter.com/zack_bitcoin/status/812921755199242240?s=20

A mathematical strategy
=========

Ouroboros is designed with the goal of being able to prove that it is secure, instead of the ad-hoc trial and error methods that most people are using to design blockchains. This is a refreshing perspective that I agree with.

That is why I came up with this system for quantifying what exactly we mean by "secure". https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md
This way it is possible to compute if one blockchain mechanism is more or less secure than another.

The proofs from the Ouroboros paper are probably all correct, I will not be attempting to disprove any of them.

The process of proving the security of a blockchain has these steps:
1) we make a model to describe what security means for our situation.
2) we prove that the mechanism we have built obeys the necessary properties defined in the security model.

Step (2) is the part we can prove in an undeniable mathematical way.

Step (1) is a controversial topic in blockchain today.
For example, we cannot even agree on which security model is securing bitcoin https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/bitcoin.md

In this paper, I will be trying to explain what security model Ouroboros is based upon, and I will try to show that their security model is not good enough to secure a cryptocurrency.

Which means that all their proofs, even though they are valid, they are proving facts that actually aren't important as to whether or not Ouroboros can secure a cryptocurrency.

So I will mostly be focusing on section 2 of their white paper.


The Security Model Goals
=========

Ouroboros security is based on the goals of persistence, and liveness.

By "persistence", they mean that the blockchain needs to have finality. if a tx gets deeper into history, we need to have increasing confidence that it wont get reverted.

By "liveness", they mean that txs wont get censored. A valid tx with sufficient fees will get included in some finite number of blocks.

As far as I can tell, these 2 goals do fully encompass the needs of a cryptocurrency.

I will be attempting to show that the Ouroboros model fails to achieve the goal of liveness.

Tx censorship is a deep problem. The ability to do arbitrary censorship is called a "soft fork". It is a kind of attack that lets the attacker make arbitrary changes to the consensus rules of a blockchain.

Voting protocols are vulnerable to bribery because of tragedy of the commons. So it should be cheap to bribe the PoS validators to participate in a censorship attack.

The Security Model
=========

The Ouroboros security model section of the white paper describes a random number generator. Since random number generation in a multiparty environment has been solved for decades, we will assume that Ouroboros can correctly re-implement one of the exiting valid solutions.

Ouroboros randomly selects stakers, weighted by how much they have staked, and gives them the opportunity to make the next block.


So, my goal in this paper is to show that even if we can select stakers at random, and we don't break the random number generator, we can still break the liveness guarantee.

This will show that the ouroboros security model is not actually providing liveness guarantees. So even if all the proofs in Ouroboros are correct, and the model is a valid description of the blockchain, it is still not secure, because all those proofs are proving something unrelated to whether or not Ouroboros can be a secure cryptocurrency.


We can pay for censorship of txs
========

Ouroboros doesn't punish block producers for failing to include a tx. The next block producer is supposed to include it instead, because they want the fee.
This means if you are willing to pay more than the sum of the tx fees for all the txs, for every block you want to censor them, a rational block producer should be willing to take your bribe and participate in the censorship.


Fork-Choice-Rule and Orphan Blocks
=========

The Ouroboros authors failed to explain the fork choice rule.
In their theoretical model, everyone will find out about the blocks in the same order.
But in adversarial environment, we need to program the nodes to handle all the exceptions. It is not acceptable to let the blockchain crash, just because messages appeared in a different order than our model has considered.

For example, an attacker might build a single valid-looking block at a moment in our recent history. We need to be able to handle that sort of block as an orphan, because people who are syncing after the fact, they cannot know the order in which blocks had appeared.

Sometimes multiple valid looking blocks will appear at nearly the same time, and different parts of the network will disagree on the timings that they had appeared, so it is a small fork that gets repaired when the next block is found.

It is necessary to program in some fork choice rule so that we can deal with orphans.


We can pay for censorship of blocks
=========

If it is possible for blocks to get orphaned, then this means that if a majority of validators worked together, they could agree to not only apply a censorship rule to the txs they include in their own blocks, they could agree to never build on top of blocks that break any of the new censorship rules.

So this means that any proof of stake block producers who did not update to participate in the soft fork, all their blocks will keep getting orphaned, they will not receive rewards until they update their node with the new soft fork code.

We can pay to change consensus rules
==========

So this shows that if a majority of block producers want to change the consensus rules, they can use censorship to do a soft fork to make this change, and everyone else needs to follow their change.

So that means, if I want to make an arbitrary change to the consensus rules of ouroboros, like giving myself a bunch of counterfeit money from nowhere, all I have to do is convince a majority of the proof of stake validators to do a soft update to add new censorship rules.

Calculating the cost to change consensus rules
=========

Lets use a 2x2 game theory matrix to calculate how much of a bribe I would need to pay the PoS validators to convince them to participate in my censorship attack:

```
B = bribe
R = block reward
D = damage due to a successful attack
P = how much more likely the attack is to succeed, if you participate

                Attack succeeds   Attack fails
Do Attack:      B+R-(D*P)         B+R
Do not Attack:  R                 R
```

Since B>0, we know that B+R>R.

To know how big a bribe we need for the attack to succeed, we need to calculate B such that B+R-(D * P)>R  -> B > (D * P)

So, if I am willing to pay a bribe bigger than D * P, then they should be willing to participate in the censorship attack.

So lets plug in some numbers to estimate the cost of this attack.
10 000 block creators each with equal stake.
market cap of $10 billion.
10% of market cap is used for block creator stake.

Lets assume this is a worst case scenario attack, so it will destroy 100% of the value on the blockchain.
And lets estimate P = 1/(number of validators).

total bribe = ($10 billion) * (0.1 portion locked in validator stake) * (1/(10000 validators)) * (1/2 of the validators) = $50 000.

So for a total of $50k in bribes, I can destroy a $10 billion blockchain.

Conclusion
=======

This shows that Ouroboros is not secure. All the proofs in Ouroboros might be correct, but they are proving a worthless model. This model is not useful to us, because it is not sufficient to show that Ouroboros could be used as a secure cryptocurrency.


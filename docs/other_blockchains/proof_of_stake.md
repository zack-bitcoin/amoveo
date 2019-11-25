Proof of Stake
========

version # 3

The goal of this paper is to show that Proof of Stake blockchain consensus does not work. We take the very general definition of Proof of stake consensus: any blockchain consensus mechanism where your influence over which block is added to the chain is proportional to some value that you own inside the blockchain's consensus state.

[Here you can see counter-arguments to this paper](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/the_defence_of_pos.md)

History
======

There have been many attempts to prove that PoS blockchain consensus does not work.
Paul Sztorc proved that PoS can't be used for distributing new tokens http://www.truthcoin.info/blog/pow-cheapest/
So PoS can not completely replace PoW.

Vitalik explained about some ways to attack PoS, and the design strategies they used to prevent these attacks https://blog.ethereum.org/2014/11/25/proof-stake-learned-love-weak-subjectivity/

Nothing at stake problem. This was an attempt to show that PoS does not work because it costs nothing to participate in all possible histories. This problem was solved by requiring validators make a deposit that can get destroyed if they participate in multiple versions of history.

Long range attack. This was an attempt to show that PoS does not work because it is always possible to rebuild history by buying up all the worthless private keys from some point in history. This problem was solved by having the validators lock money for long enough periods of time, so that their private key isn't worthless for that time period. This way there is a long enough period of time so that enough of the participants in the network can sync recent blocks so that we can have consensus. Basically, it is a checkpointing system.

Incentives problem. This was an attempt to show that PoS does not work because it is impossible to align the incentives of the validators with the success of the network. This problem was solved by integrating futarchy elements into the PoS consensus.

moralistic enforcement
==========

Here is a paper from 1992 explaining why PoS cannot work. http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.405.286&rep=rep1&type=pdf
"If the costs of being punished are large enough, moralistic strategies which cooperate, punish noncooperators, and punish those who do not punish noncooperators can be evolutionarily stable. We also show, however, that moralistic strategies can cause any individually costly behavior to be evolutionarily stable, whether or not it creates a group benefit."

The problem with PoS is that since the validator's value is internal to the system, an attack can do second-order punishments. The attack can punish validators who fail to participate in enforcing punishments. And this allows for arbitrary changes to the protocol to be dominant.

POS systems all require some logical connection between value in the system, and the decisions about which blocks to add to the blockchain.
If we are to solve the nothing-at-stake and long-range attack problems, there needs to be a lot of money locked up in every consensus level decision.
So in POS systems, it is always possible for a majority coalition to punish accounts who don't participate in the consensus in the way the majority wants. The majority simply censors them from accessing their money any more.
So in POS systems, once a majority coalition has formed, it is very stable. It is easy for the participants in the coalition to trust each other, because if any one of them cheats, the rest of the coalition can tell who cheated and punish them by freezing their money.
So the majority coalition can use soft forks to push through any changes to the consensus rules that they want, they can steal any money, they can raise the fees as high as they want.

Censorship can be good
=========

A fundamental property of POW is that it is impossible to punish censorship. Miners don't necessarily have any currency in the system, so it just isn't possible to punish them for not including txs.

This is different from saying "pow doesn't punish censorship". In POW, it is impossible to do a soft fork to add the ability to punish censorship.

This has long been seen as a weakness in POW. People like to imagine that censorship is bad, that we should punish miners for censoring. But this is actually a critically important property of POW consensus that gives it security against certain attacks, and we will show that any blockchain consensus without this property is less secure than centralized alternatives.

In POW systems there is no connection between value in the system and miners. Many miners don't even own any coins. The only thing that a majority coalition can do is to censor blocks. Forming a majority coalition of hashpower to control the blockchain costs C amount to coordinate the different miners, and it pays out P amount of extra rewards per miner. But, since it is impossible to punish censorship, a sub-coalition of the coalition could coordinate to use censorship take control, and the first coalition has no way to punish the sub-coalition. The sub-coalition have half as much hashpower to coordinate, so their cost is C/2. and they are sharing the rewards between half as many miners, so their payout of extra rewards is 2*P.

In POW, it is always easier and more profitable to create a sub-coalition to overthrow a coalition than it was to create the first coalition. This fact makes it so that no miner is interested in forming coalitions, because they know it is probably a trick to steal their hashpower.

From the miner's perspective, if they participate in your coalition they will probably lose all their profits.

cost of the bribe to initiate the attack
========

If you are a validator who thinks the attack will fail, then you are comfortable taking the bribe. Because you know the attack will fail either way, so you might as well take the free money.
If you think the attack will succeed, then you will sell your coins to someone who thinks the attack will fail.

According to [tragedy of the commons](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md), the cost to bribe the validators to form a majority coalition and destroy the blockchain is:
```
LU = (how much the validators have to lock up)
#V = (how many validators are there)
Bribe = LU / (2 * #V)
```

Realistically, PoS blockchains are designed to have >100 validators, and to lock up as much as 90% of the stake. In that case, plugging into the formula above: `Bribe = (0.9 / (2 * 100)) = (0.45% of the market cap)`

If it costs less than 1% the value of a blockchain to destroy that blockchain, this is even less secure than the cost of the legal efforts to shut down a centralized alternative.

In order for PoS to achieve level 3 trust, the cost of bribing the validators would need to be bigger than the potential profit from controlling consensus. [you can read about trust levels here](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md)
```
PP = (potential profit form controlling the consensus)
LU / (2 * #V) > PP
```

Lets make the reasonable assumption that there are more than 10 validators

```
LU > (20*PP)
```

The potential profit from controlling the system includes all the value in the system, including LU. So we know that:
```
PP > LU
```

It is impossible to make both of these inequalities true at the same time.
This shows it is impossible for PoS to achieve level 3 trust, it will always be level 4.


Tragedy of the Commons details
===========

How can we be sure that the tragedy of the commons model is the correct way to model the cost of the bribe?

In mechanism design, a fundamental assumption is that users prefer having more value instead of less. This means we can calculate user behaviour, and thus, we can calculate if it is a secure mechanism.

So lets put ourselves in the shoes of a PoS coin holder, and imagine their situation.

Someone is publicly giving out bribes to vote for the blockchain's destruction.
I don't know how many other people have signed up for the bribe.
If I sign up, and the attack fails, I can keep the bribe and am not punished.

Lets model the value of your coins.
We know that if the attack succeeds, the coins are worth nothing.
Since you don't know how many other people have accepted the bribe, you have to model the value of your coins like this:
Value = (value if the attack fails) * (likelihood of attack failure)

So, if we want to calculate the cost to the user, if they take the bribe, it would be:
Value(no bribe) - Value(bribe) = (value if the attack fails) * (how much more likely the attack is to succeed, if you take the bribe)

(how much more likely the attack is to succeed, if you take the bribe), another way of saying that is (the likelyhood that your vote is pivotal), another way of saying that is (your influence of the outcome).

So we need a way of modeling how much more likely the attack is to succeed if you take the bribe.
According to the laws of probability, mutually exclusive outcomes need to add up to 100%. If we sum up how much control each of the validators for the PoS blockchain has, it needs to add up to 100%.
One way to get all their influences to add up to 100%, is if each individual's influence is (their stake)/(total stake), which is the same as (the portion of the stake that this user controls).

This is called the "market failure model", or the "tragedy of the commons model".

`(Bribe for one user) = (value of their coins) * (portion of stake that they control)`

`(Bribe for 1/2 of stake) = (value of 1/2 of stake) * (portion of stake controlled by the average staker being bribed)`

So, plugging in some example values.
If there are 1000 validators, and the blockchain is worth $1 billion, and 90% of the value is staked, then the total cost to bribe >50% of the validators would be: `($1 billion) * (0.9) * (1/2) * (1/1000) => $450 000`

So less than $1/2 million in bribes is sufficient to completely destroy a $1 billion PoS blockchain.
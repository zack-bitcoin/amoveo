Proof of Stake
========

The goal of this paper is to show that Proof of Stake blockchain consensus does not work. We take the very general definition of Proof of stake consensus: any blockchain consensus mechanism where your influence over which block is added to the chain is proportional to some value that you own inside the blockchain's consensus state.

History
======

There have been many attempts to prove that PoS blockchain consensus does not work.
Paul Sztorc proved that PoS can't be used for distributing new tokens http://www.truthcoin.info/blog/pow-cheapest/
So PoS can not completely replace PoW.

Vitalik explained about some ways to attack PoS, and the design strategies they used to prevent these attacks https://blog.ethereum.org/2014/11/25/proof-stake-learned-love-weak-subjectivity/

Nothing at stake problem. This was an attempt to show that PoS does not work because it costs nothing to participate in all possible histories. This problem was solved by requiring validators make a deposit that can get destroyed if they participate in multiple versions of history.

Long range attack. This was an attempt to show that PoS does not work because it is always possible to rebuild history by buying up all the worthless private keys from some point in history. This problem was solved by having the validators lock money for long enough periods of time, so that their private key isn't worthless for that time period. This way there is a long enough period of time so that enough of the participants in the network can sync recent blocks so that we can have consensus. Basically, it is a checkpointing system.

Incentives problem. This was an attempt to show that PoS does not work because it is impossible to align the incentives of the validators with the success of the network. This problem was solved by integrating futarchy elements into the PoS consensus.


Censorship can be good
======

A fundamental property of POW is that it is impossible to punish censorship. Miners don't necessarily have any currency in the system, so it just isn't possible to punish them for not including txs.

This is different from saying "pow doesn't punish censorship". In POW, it is impossible to do a soft fork to add the ability to punish censorship. The network will always follow the side with more work done.

This has long been seen as a weakness in POW. People like to imagine that censorship is bad, that we should punish miners for censoring. But this is actually a critically important property of POW consensus that gives it security against certain attacks, and we will show that any blockchain consensus without this property is less secure than centralized alternatives.

In POW systems there is no connection between value in the system and miners.
Many miners don't even own any coins.
The only thing that a majority coalition can do is to censor blocks.
Forming a majority coalition of hashpower to control the blockchain costs C amount to coordinate the different miners, and it pays out P amount of extra rewards per miner.
But, since it is impossible to punish censorship, a sub-coalition of the coalition could coordinate to use censorship take control, and the first coalition has no way to punish the sub-coalition.
The sub-coalition have half as much hashpower to coordinate, so their cost is C/2. and they are sharing the rewards between half as many miners, so their payout of extra rewards is 2*P.

In POW, it is always easier and more profitable to create a sub-coalition to overthrow a coalition than it was to create the first coalition.
This fact makes it so that no miner is interested in forming coalitions, because they know it is probably a trick to steal their hashpower.

From the miner's perspective, if they participate in your coalition they will probably lose all their profits. So the size of the bribe you would need to pay is about the same as the cost of renting the hashpower and mining yourself.

Why PoS fails
======

POS systems all require some logical connection between value in the system, and the decisions about which blocks to add to the blockchain.
If we are to solve the nothing-at-stake and long-range attack problems, there needs to be a lot of money locked up in every consensus level decision.
So in POS systems, it is always possible for a majority coalition to punish accounts who don't participate in the consensus in the way the majority wants. The majority simply censors them from accessing their money any more.
So in POS systems, once a majority coalition has formed, it is very stable. It is easy for the participants in the coalition to trust each other, because if any one of them cheats, the rest of the coalition can tell who cheated and punish them by freezing their money.
So the majority coalition can use soft forks to push through any changes to the consensus rules that they want, they can steal any money, they can raise the fees as high as they want.

According to [tragedy of the commons](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md), the cost to bribe the validators to form a majority coalition and destroy the blockchain is:
```
LU = (how much the validators have to lock up)
#V = (how many validators are there)
LU / (2 * #V)
```
Which is less than 1% of the value on the blockchain for all POS designs I have seen.

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
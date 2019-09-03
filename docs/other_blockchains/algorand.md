Algorand Review
===========

Algorand is a PoS blockchain consensus protocol. Here is the paper describing its design https://algorandcom.cdn.prismic.io/algorandcom%2Fece77f38-75b3-44de-bc7f-805f0e53a8d9_theoretical.pdf

I have attempted to show that proof of stake is impossible https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

So the goal of this paper is to use what I have learned about PoS, and to see if Algorand is a counter-example of my conclusions, or if I can show that Algorand is not secure.

Some quotes from Algorand's paper
===========

"Algorand withstands a very powerful Adversary, who can instantaneously corrupt any user he wants, at any time he wants, provided that, in a permissionless environment, 2/3 of the money in the system belongs to honest user."

"PKr: the set of public keys by the end of round r−1 and at the beginning of round r."

"Honest Majority of Users Assumption: More than 2/3 of the users in each PKr are honest."

Based on these quotes from the white paper, it seems like an attacker needs only to convince more than 1/3rd of of the money holders to participate in his attack.

So, if we can show that it is possible to cheaply bribe more than 1/3rd of the money holders to participate in an attack, then that means Algorand is not secure.

But, we are going even further.
We will try to show that it is cheap to bribe >2/3rds of money holders. That way the attacker isn't just causing Algorand to freeze, the attacker can change any of the rules defining Algorand. They can steal any money. They can switch it to be a PoW blockchain.

Bribes are cheap.
=========

Bribing users to participate in an attack [is very cheap](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md).

Lets use a 2x2 game theory square to calculate exactly how big the bribes would need to be to destroy a blockchain like Algorand.

```
B = Bribe
R = Block Reward
D = damage due to a successful attack
P = how much more likely the attack is to succeed, if you participate
Attack result:    Success     Failure
      you attack:  B+R-(D*P)  B+R
you don't attack:  R          R
```

Since B>0, we know that B+R>R.

To know how big a bribe we need for the attack to succeed, we need to calculate B such that B+R-(D * P)>R  -> B > (D * P)

So, if I am willing to pay a bribe bigger than D * P, then they should be willing to participate in the censorship attack.

So lets plug in some numbers to estimate the cost of this attack.
1 billion users with approximately equal stake.
market cap of $1 quadrillion.

Lets assume this is a worst case scenario attack, so it will destroy 100% of the value on the blockchain.

And lets estimate P = 1/(number of users).

total bribe = D * P = ($1 qudrillion) * (1/(1 billion validators)) * (2/3 of the validators) = $666.

So, for a total cost of $666 in bribes, I can destroy a $1 quadrillion blockchain.

Soft forks
=========

A soft fork is a way to update a blockchain that does not break any existing rules. It only adds new rules to censor some kinds of blocks that were previously considered valid.

Any change to the consensus mechanism rules can be done using a soft fork. So soft fork attacks are very dangerous. They can redistribute the money, or change the blockchain to use PoW for security, or anything they want.

The attack
=========

The attacker would release an alternative version of algorand software that pays an extra reward to anyone who is using the new software to participate in staking.

Once more than 2/3rds of the money holders are using the new version of the software, then the soft fork activates, and it becomes impossible to make the old kinds of txs that we have all agreed to censor.


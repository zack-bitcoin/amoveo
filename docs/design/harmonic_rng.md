Harmonic Random Number Generator
============

It would be nice if we could securely generate random numbers inside of a blockchain. This would allow for things like probabilistic value transactions, lotteries, and probabilistic-value sharding for scalability.
The goal of this paper is to show how to do this.

A simple example of a broken RNG
===============

Lets start by considering some simple example RNG, and find out why they do not work.
One simple kind of RNG is to have every block add 1 bit of entropy.
Which means that when we generate our random number, we take the final bit from the hash of the most recent 256 headers, which makes 32 bytes. These 32 bytes are the seed to generate as much randomness as we need.

The problem with this strategy is that whoever mines the very last block before the RNG is generated, that person will have 1 bit of influence in the outcome, and it only costs them 1/2 of a block reward.
1 bit of influence can, at most, give an attacker an extra 25% control of the outcome of the contract.
So this simple example can only be secure if the amount of value controlled by the RNG is less than 2 block rewards.

How about the case where an attacker finds the second-to-last block before we generate RNG. How much profit can an attacker earn by being able to use their influence to try and win?
In this case, the attacker doesn't know what bit will be generated from the next block, so they have less information available to know if re-mining the block is helpful to them. The attacker only gets about 25%/2 influence for finding this block.

In this simple example where every block is generating 1 bit of entropy, the very last block gives 25% influence, and the second to last block is giving less influence.
Since the very last block has such a large amount of influence, that is our weakness.

Harmonic RNG
========

A basic motivator for the harmonic RNG design is that all the blocks should have equal influence over the outcome. That way a miner who finds the last block has no advantage over the miner who finds the 2nd to last, or 3rd to last.

in order to achieve this, we need to skew the probability that a block will add a "1" or a "0" to the entropy.
we call these probabilistically skewed bits partial-bits (p-bits).

The probability that each p-bit is a 1 decreases according to the harmonic sequence.

So lets show that it actually is the harmonic sequence.
Suppose one value 3/35 is the last value of the sequence, what should the second to last be so they both have the same influence?

Whoever can choose 2nd to last, they will assume that the last value will be a 0, because there is 32/35ths chance it will be.
so the second to last person is 32/35ths as likely as the last person to make the right choice.
So we need the 2nd to last value to be (3/35)/(32/35), which is (3/32)

so the normal sequence is like 3/6, 3/9, 3/12...

Notice that if we take smaller steps like this:
3/6, 3/7, 3/8... that everyone has less influence than the previous. So whoever is last in this sequence has the most influence.
As long as the person who goes last has a small enough influence to be tolerable, then everyone has a small enough influence to be tolerable.

```
P(N) = the probability that block N produces a "1".
P(N) = C/((2 * C)+N) (for some constant C).

P(N) is the generic form of a "harmonic sequence".
```

Approximating the influence from each block
===================

If the harmonic sequence is B blocks long, then the amount of influence of each block is I(N).

```
R(N) = (prob rest are 0 after N-1)

I(N) = P(N) * R(N+1)

R(N) = (1-P(N)) * R(N+1)... = PI from N to B of (1-P(N))
= PI from x=N to B of (C+x)/(2C+x)
= ((C+B)!/(C+N-1)!)  / ((2C+B)!/(2C+N-1)!)
= (C+B)!(2C+N-1)! / (C+N-1)!(2C+B)!
assuming C<<B
approx= ((N)/(B))^C

I(N) ~= ((N/B)^C) * C/((2 * C)+N)
```

re-roll the "1"'s attack
===========

before we considered the attack where a miner rerolled the very last block of the sequence.
The next attack we consider is this:
every time a miner finds a rare "1" bit, that means if they reroll, the odds that they will change it to a "0" is very high.
So we need to consider an attacker who is willing to reroll every block that turns up a "1" in the harmonic series.
Such an attacker is able to get 1 bit of influence over the outcome.

`BR = block reward`
The cost of this is
```
(BR/2) * (sum from n=1 to B of C/(2C+N) =
(BR/2) * C * log(2C+N) from 0 to N = 
(BR/2) * 2C * (log((2C+N)/2C)) =
BR * C * log((2C+N)/2C)
```

So the number of 1 bits is C * log((2C+B)/2C)
Each of these has much less influence than the final block=P(B)
So the influence of the attacker is less than =
C/(2C+B) * C * log((2C+B)/2C) = c * c * log((2c+B)/2c)/(2c+B)

(using example C=21, N=1000)
21 * 21 * log(1042/42)/1042 = 1.35 rerolls if each block was a full bit of entropy. 

Optimizing the constants
=====================

We are free to set C and N as any positive integer.
Our goal is to select C and N such that the security against both kinds of attacks is equal, that way we don't have any weak spots in our armor.
N is a number of blocks. We want this to be a short enough period of time so that users are willing to only use contracts that last longer than this period of time.

So, lets set the equations for the profitability of the 2 attacks as equal to each other, and find some example C,N pairs that we could use.

```
(cost due to 1-bits in the series to be rerolled) = (reroll very last block) =>
BR * C * log((2C+N)/2C)/2 = BR * (1/(C/(2 * (C+N)))) =>
BR * C * log((2C+N)/2C)/2 = BR * (2 * (C+N))/C =>
C * C * log((2C+N)/2C) = 4 * (C+N)
```

```
Time period to generate entropy, N=number of blocks in the harmonic sequence, C, security ratio = how many times more secure this is than 1 bit per block = ((2 * C)+N)/C 

1 week,   1000, 21, 49.6
2 weeks,  2000, 28, 73.4
1 month,  4000, 38, 107.3
2 months, 8000, 51, 158.9
```

It seems like doubling the length of time that we use for generating entropy increases the amount of security by about 50%.

Parallel Sortition Contracts
================

If we split up 1 big sortition contract into 100 little ones, then the cost/benefit ratio of an attack increases by a factor of 100x.

If the most value we can fit into a sortitoin chain is only $10k, but we want $100k of contracts, then we just need to make 10 sortition chains.

(Maximum value in sortition chains) = constant * (# of sortition chains) * (size of the block reward)

Generating multiple Pbits per block
================

Maybe if we generate multiple pbits per block, it could be more efficient, so we need fewer blocks to generate the entropy.













<!----

expired notes


C bits, N blocks.
# of rerolls required to attack
=> integral from 0 to N of ((C/2)/(C+N)) dN
=> (C/2)*log(C+N) from 0 to N
=> (C/2)*(log(C+N) - log(C))
=> (C/2)*(log((C+N)/C))

expected profit of the ability to reroll is the same for every block: C/(2*(C+N))

There are 2 kinds of attacks we need to be secure from:
1) we don't want it to be profitable to reroll only the very last block.
2) we don't want it to be profitable to possibly reroll the blocks that come up as a 1 bit.


we want to optimize both so that both kinds of attack are equally expensive which means C and N are related this way:
(# rerolls) * (expected profit) = 1
((C/2)^2)*log((C+N)/C)/(C+N) = 1

(C/2)*(log((C+N)/C)) * 10*C/(2*(C+N)) = 1
10*((C/2)^2)*log((C+N)/C)/(C+N) = 1

security = 2*(C+N)/C = 2*(17+4000)/17 = 943

There is no simple formula relating C and N, so here are some examples.
If a sortition chain lasts 4 months, then N is about 16000, so we should set C to 114, and the security is 283x higher than normal.
If it lasts 2 months, then N is about 8000, so we should set C to 84, and the security is 192.5x higher than normal.
if it lasts 1 month, then N is 4000, and C should be 62, and the security is 131x higher than normal.
if it lasts 2 weeks, then N is 2000, and C should be 47, and the security is 87 times higher
if it lasts 1 week, then N is 1000, and C should be 35, and the security is 59x higher
if it lasts 1 day, then N is 144, and C should be 17, and the security is about 19x higher.

Time, many blocks, many bits, how much higher is security
4 months,16000,114, 283
2 months, 8000, 84, 192.5
1 month,  4000, 62, 131
2 weeks,  2000, 47, 87
1 week,   1000, 35, 59
1 day,     144, 17, 19


It seems like doubling the length of time that we use for generating entropy increases the amount of security by about 50%.





Instead of smearing 1 bit of entropy out into 1000 blocks, we should smear the last 30 bits over those blocks.
so the probability that block N produces a 1 bit instead of a 0 bit should be (15/(N+30)).
The expected profit of being able to reroll a block in that case is a constant 15/(1030), which is less than 1/50th what it was in the case where we generate 1 bit per block.

And if we specifically check for rerolling only on those occasions where a 1 bit was randomly found: https://www.wolframalpha.com/input/?i=sum+from+n%3D1+to+1000+of+15%2F%2830%2Bn%29
It means we would have to re-mine about 26 blocks in order to get 1 bit of leverage over the outcome.
So the loss due to this attack is 1/26th as much as if we generated 1 bit of entropy per block.

So this is a technique that allows at least 50x fold increase in security against miners rerolling, and I believe I can get this up to 100x if I play around with the constants some more.





Another trick I found is based on http://mathworld.wolfram.com/SultansDowryProblem.html
Basically, if we re-use the exact same generated entropy to determine the outcome of many different similarly sized contracts, then the miners aren't able to effectively reroll attack all those different contracts simultaneously.

if we have N different contracts reusing entropy, the profit distribution for miners is a bell curve with a standard deviation of sqrt(N)/2

If we have N contracts reusing the same entropy, the profitability of a reroll attack decreases by 1/sqrt(N).


Combining these two tricks, it seems like we have at least a 1000x increase in the cost to do reroll attacks, which means we can safely put 1000x more money into our probabilistic contracts without becoming vulnerable to reroll attacks.







prob_1(n) = ?

profitability(n) = prob_1(n)*all_zeros(n+1).

all_zeros(n) = (1 - prob_1(n))all_zeros(n+1).



10/20  log_20(N)

10/(20+n)

profitability(n) = (10/(20+n))*all_zeros(n+1).

all_zeros(n) = ((10+n)/(20+n))*all_zeros(n+1).

all_zeros(n) = ((n+19)! / (n+9)!)/((n_final+20)!/(n_final+10)!)

if we use 1000 blocks of time to gather entropy:

all_zeros(n) = ((n+19)! / (n+9)!)/((1020)!/(1010)!)

profitability(n) = 10*((n+19)! / (n+10)!)/((1020)!/(1010)!)
=10*(1010!/1020!)*((n+19)!/(n+10)!)





prob_1(n) = 1/n
prob_0(n) = (n-1)/n

profitability(n) = (1/n)*all_zeros(n+1).

all_zeros(n) = ((n-1)/n)*all_zeros(n+1).

all_zeros(n) = (n-1)/n_final.

profitability(n) = (1/n)*((n)/n_final) = 1/n_final.

sum(n=0, 100, profitability(n)) = approx integral from 0 to 1000 of 1/1000 dn

--->
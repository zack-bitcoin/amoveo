


A proof using projective geometry that the sequence we want is the harmonic sequence
==================

A theoretically maximally secure RNG would give an amount of security based on the expected value of block rewards during the period of time when entropy is being generated.

So, the amount of certainty about what entropy we are generating would be increasing linearly with the expected block rewards.
If it is increasing linearly, then it is a projective transform of the nonzero integers.

A more specific projective proof that P1(n) must be a projective transformation
===============

by projective transformation, I mean a homography https://en.wikipedia.org/wiki/Homography

In order for the amount of certainty created per block to be constant, we need the amount of certainty of each block to be a projective transform of the amount of certainty in the remaining blocks to gather all the entropy.

the amount of entropy left to gather is a projective transform of the amount of entorpy already gathered.

the amount of certainty of a pbit is a projective transform of the amount of uncertainty of that pbit = the probability of a 1 pbit for each block.

The probability of a 1 pbit on each block must be some projective transformation of the amounts of certainty left to accumulate. 

Which is a projective transformation of the amounts of certainty accumulated so far.

Any combination of projective transformation is another prpojective transformation.

Therefore, the probability of a 1 on each pbit must be some projective transformation of the nonzero integers.

a more symmetrical projective proof that P1(n) must be a projective transformation
=============

The amount of certainty is independent of the starting amount of entropy, the starting height, the block reward per block, or how much entropy is left to gather. All that matters is the total expected block rewards paid out so far.

The only way P1(N) can be invarient of all these symmetries is if it is projective transformation of the amount of certainty accumulated so far.

now that we know it is projective, show the result
===========

And the amount of certainty is a projective transformation of the integers 0,1,2...

`P1(0) = 1/2`. because this is where we start generating entropy, it is the degenerate case.
P1(N) should always be < P1(N-1).

A projective transformation that maps the integers 0,1,2... onto the range [1/2, 0), is the harmonic sequence 1/(2+N).

According to the fundamental theorem of projective geometry, this projective transformation must be the unique solution https://en.wikipedia.org/wiki/Homography


A construction to prove the result is correct
============

put the domain on the points [0, 2+Y]. for Y in the non-negatives.
put a focus at (1,0).
project the range through the focus onto the line [X+1, -1].

The relationtionship between Y and X:
Y(n) = n, X(n) = 1/(2+n)

So the harmonics are uniquely determined.


<!----

y = x*x

-3, 9
-2, 4
-1, 1
0, 0
1, 1
2, 4
3, 9

CR = (a+b)*(b+c)/b/(a+b+c)
a,b,c = 1, 3, 5

4*8/3/9 = 32/27 = 1 + 5/27




P1(N)*reward1(N) = BR/2

influence(0) = P1(0) * reward1(0) + P0(0) * reward0(0) = reward1(0)/2 + reward0(0)/2 = BR

influence(1) = P1(1) * reward1(1) + (1-P1(1)) * (BR/2)
= BR/2 + P1(1)*(reward1(1) - BR/2) = BR
-> reward(1) = BR/(2*P1(1)) + BR/2
= (BR/2)(1 + 1/P1(1))
-> BR/2/P1(1) = (BR/2)(1+1/P1(1))
-> 1 = P1(1)*(1+1/P1(1))
-> 1 = P1(1) + 1

in order for the certainty to increase linearly, each block needs to increase the certainty by the same amount. Which means that the probability of a 1 pbit needs to decrease as we gain confidence in what entropy will get generated: `P1 =  1/(1+entropy generated so far)`.

So P1 is the projective transformation moving points on the y-axis (0, Y+2) to points on the line (X+1, -1) by drawing a pencil of lines through the point (0, 1).

Since P1 is a projective transformation, it is preserving the cross-ratio of all the points being transformed.

here are example points being transformed projectively:
(0,2),     (0,3),     (0,4),     (0,5) ->
(3/2, -1), (4/3, -1), (5/4, -1), (6/5, -1)

Which is transforming from Y to X like this:
0, 1, 2, 3 -> 1/2, 1/3, 1/4, 1/5

so the range of the transformation is integers Y in [0, infinity)
and the domain of X is a subset of [1/2, 0)

Since it is a projective transformation, the cross ratio should be preserved. Lets check that this is the case

given 3 colinear line segments a, b, and c, the cross ratio CR is:
CR = (a+b)*(b+c)/b/(a+b+c)

cross ratio1 =
(1+1)*(1+1)/1/3 =
2/3

cross ratio2 =
(1/6 + 1/12)*(1/12 + 1/20) * 12 / (3/10)
= (1/4)*(8/60)*12*10/3
= 2/5*10/3
= 2/3

so the cross ratio works.

The only possible sequence that is invarient under perspective projection is the sequence of harmonics.

The only possible sequence that we can make in a finite range, which is a projective transformation of an infinite line, is the harmonic sequence * available range.





So there are invarients in the sequence according to a perspective projection where the point -1 is the vanishing point. Because the magnitude of P1 decreases as 1/x.

It doesn't matter how long the block time is. Only the accumulation of rewards.
So it is time dilation invariant.

It doesn't matter what block the entropy gathering process starts on. So it is time translation invariant.



CR = (a+b)*(b+c)/b/(a+b+c)

example 1/2, 1/3, 1/5
(1/2 + 1/3)*(1/3 + 1/5)*3/(1/2 + 1/3 + 1/5) =
(4/6)*(8/15)*3/(31/30)
4*8*3*30/(6*15*31) =
16*30/(15*31) =
16*2/(31) =
32/31 = 1 + 1/31

example 1/2, 1/3, 1/4
(1/2 + 1/3)*(1/3 + 1/4)*3/(1/2 + 1/3 + 1/4) =
(4/6)*(7/12)*3/(13/12)=
4*7*3*12/(6*12*13)=
2*7/13= 14/13 = 1 + 1/13

example 1/2, 1/4, 1/8
(1/2 + 1/4)*(1/4 + 1/8)*4/(1/2 + 1/4 + 1/8) =
(3/4)*(3/8)*4/(7/8)
3*3*4*8/(4*8*7) =
3*3/7 = 1 + 2/7


A proof that it is Harmonic RNG from projective geometry
==================

An ideal RNG could only be as secure as the accumulated block rewards of all the blocks built to generate the entropy.

It doesn't matter how long the block time is. Only the accumulation of rewards.
So it is time dilation invariant.

It doesn't matter what block the entropy gathering process starts on. So it is time translation invariant.

it doesn't matter how big each block reward is, only the total accumulation of rewards. so it is reward dilation invariant.

it doesn't matter how much money or hashpower the participants have, so it is reward translation invariant.


Lets say 3 probabilities P(N) are a, b, and c. 
CR = (a+b)*(b+c)/b/(a+b+c)

example 1/2, 1/3, 1/5
(1/2 + 1/3)*(1/3 + 1/5)*3/(1/2 + 1/3 + 1/5) =
(4/6)*(8/15)*3/(31/30)
4*8*3*30/(6*15*31) =
16*30/(15*31) =
32 / 31




example 1/2, 1/4, 1/8
(1/2 + 1/4)*(1/4 + 1/8)*4/(1/2 + 1/4 + 1/8) =
(3/4)*(3/8)*4/(7/8)
3*3*4*8/(4*8*7) =
3*3/7

example 1/2, 1/8, 1/16
(1/2 + 1/8)*(3/16)*8/(1/2 + 3/16) =
(5/16)*3/(11/16) =
3*5/11

example 1/2, 1/4, 1/16
(3/4)*(5/16)*4/(13/16)
=3*5*4*16/(4*16*13)
=3*5/13

CR*b/(b+c) = (a+b)/(a+b+c)
CR*b/(b+c) - 1 = -c/(a+b+c)
(CR*b - b - c)/(b+c) = -c/(a+b+c)
(a+b+c) = c*(b+c)/(b + c - CR*b)
a = (c*(b+c)/(b + c - CR*b)) - b - c



a+b+c = (b+c)/(CR - b - c)
  u=b+c
a+u = u/(CR-U)
a = u(1/(CR-U) - 1)
a = u(1+u-CR)/(CR-U)




It doesn't matter if the block_time/block_reward is changing by some constant factor on every block.
so the cross ratios are invariant to projective transformations in the time-reward plane.

Therefore, it must be the harmonic sequence. Since only the harmonic sequence is projective invariant.









Harmonic Resets
==============

We want it to be hard for the attacker to calculate how much more profitable a reroll will be.
To do this, we can have every 1-pbit reset the cycle back to B(0). So instead of taking N steps to gather entropy, it will take N*log2(N).

So now the ability to roll a 1-pbit is not much power at all. There is only a 1/N chance that it will be the last 1-pbit.

And the decision-making power a miner gets from not publishing a 1-pbit is none.
only publishing 0-pbits is going to have an impact.
The fact that you won a 1-pbit and did not publish it, this doesn't lower the probability that others will also find 1-pbits.

And when you do publish a 0-pbit, it only has a very small measurable impact on the outcome.

the combination of harmonic rng with harmonic resets means that with have Nx higher security, but it takes N * log2(N) times longer to settle the sortitoin chain.

harmonic resets gets rid of 1/2 the available info when deciding whether to reroll, so it reduces the loss due to attacks by 2.

Variable block reward optimization
=============




Slow block optimization
============

if each block was an 110 minutes long, then we could make the block reward 10x bigger, so it would cost 10x more to reroll attack.


off-chain score method
=============

instead of calculating the winner on-chain directly, this strategy would allow each sortition chain user to calculate their score in private. When the sortition chain is done, we can compare our scores to see who won.

This is a 3x improvement in how much value we can store in sortition chains.

sortition chains in parallel strategy
==============

the blockchain can have many sortition chains running at the same time. 1000 should be easy to maintain with the current software.
1000x improvement in how much value we can store in sortition chains.




having different sortition chains share an entropy source
================


I feel like if there are multiple sortition chains that are getting their entropy from the same block height, it must reduce an attacker's ability to influence those sortition chains in their favour. Because optimizing one will limit their ability to optimize the other.

It is weird though, because this trick is decreasing the frequency of attacks, which is something we want. But it is also decreasing the cost of attacks, which is something we want to avoid.
it creates more situations where an attacker has the ability to influence 2 sortition chains simultaneously in their favour, using only 1 reroll.

if there are S sortition chains being settled at the same block height, I guess an attacker could influence sqrt(S) of them with a single reroll.
Which means S-sqrt(S) are free of influence

If there are 1000, 1000-sqrt(1000) is about 968.
so 97% of the sortition chains would be unaffected.



What if 1/1000 blocks has extra high difficulty and reward, and we use that block for entropy?
=============


combining all these improvements:
===========
(starting example) * (harmonic rng) * (harmonic-resets) * (slow blocks) * (off-chain score) * (sortition chains in parallel) * (sharing an entropy source between 300 sortition chains)


block reward = 0.1 veo.
cycle length = 100 (so it takes 1000 blocks to settle on average).

2 * 4 * 2 * 1 * 3 * 1000 * sqrt(300) =
Security Constant

831 thousand veo in the sortition chains at one time.

which means we could support about 831 veo in each sortition chain.


so we need to keep the (block reward)>((market cap)/(SecurityConstant*(# of sortition chains we can handle in parallel))).

and maybe we will find a way to make the Security Constant into an even bigger number.



Security for lotteries
=========

a pure lottery has different security needs. We can't get any benefits from reusing entropy, or from parallelizing it.
so if the cycle length is 100, meaning it takes 1000 blocks on average to settle, then Security Constant =
2*4*2*1*3 = 48.

So the very biggest pure lottery we could handle with this design would be for about 48 VEO.





How much entropy is created by a harmonic rng process?
==============
C = cycle length
log2((sqrt (2))^log2(C)) =
log2(sqrt(c)) =
log2(c)/2

if C=100, then we get at least 5 bits of entropy.

How much entropy for harmonic rng with resets?
=============

if we include resets, the entropy is at least:
(prev_calc)+(number of cycles) =
log2(sqrt(C)) + log2(C) =
log2(C^(3/2)) =
(3/2)*log2(C)

So harmonic resets is providing at least 3x as much entropy vs normal harmonic RNG.







Maybe we can find a way to use the harmonic RNG process to generate each pbit inside of a harmonic RNG process?
================

the harmonic process, it takes B steps, and increases security log2(B).
We can have even more time to spare, I am thinking we can layer this inside of itself.

the entire process is generating 1 bit, but along the way it make a bunch of p-bits.
I am thinking we can use the entire harmonic process for each of the pbits.

so it would take Bx to generate the entropy, but it would provide security of log2(B)*log2(B). if we got it to work, it would work out to a factor of 3x improvement.









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
Such an attacker is able to get around 1 bit of influence over the outcome.

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
21 * 21 * log(1042/42)/1042 = 1.35 rerolls.

So even if the attacker has the freedom to reroll every "1", this is only as much influence as being able to reroll less than 1.35 times in the naive case.

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







The probability of generating pbits should depend on previous pbits generated.
================================

How about this: we will have some expiration date at height H.
For every block before H, we generate 1 bit of entropy.
lets call block H as B(2), and block H+N as B(N+2).
the probability that the pbit from B(N) is a 1 is 1/N.

If we ever get to B(11), then we are done generating entropy.

If we generate a p-bit of "1" for any B(N), then we start the process over at B(0).

So if you roll a 1, then you can reroll and you will almost certainly get a 0, but there is only a 1 in 10 chance that your "1" is the last "1".

So an attacker would have to do 10x as many rerolls in this set up in comparison to attacking the harmonic series with a concrete expiration date.

The expected amount of time between the expiration date, and when we can get our money out of the sortition chain is:
10 cycles * average cycle length = 
...
I think the average is something like 10/3, or sqrt(10) or log(10).
1/2 the cycles take 1 block.
1/3rd of the remainder take 2 blocks.

so we can make an upper bound of 
1/2 + (1/3)*(1/2)*2 + (1/3)*10 -> 1/2 + 1/6 + 3.3 -> about 4.

Looks like it is following sum(1/n), which is approximately the same as log2(n).

So I think the expected number of blocks till expiration will be 10*log2(10) for this example.

So if we are willing to wait on average N*log2(N) blocks to get our money out after expiration, then we can increase the level of security by Nx.

if N=100, then it takes about 700 blocks on average, about 5 days, to get our money out of the sortition chain, but we reduce the losses from attacks by 100 fold.





So lets consider it from a concrete game theory perspective.
An attacker has 30% of the hash power, and 50% control of a sortition chain's value.

They will win a sortition chain if the current random number is selected, so they want that all the pbits are zeros.

If this attacker is willing to do rerolls, how much money needs to be in the sortition chain for this to be profitable?

include the cost of locking up 1/2 the money in the sortition chain.

If the attacker does not interfere, the odds that this is the last cycle and that they will win is about 1/n.

The odds that only one 1-pbit would be found during a cycle of length N is
sum from i=1 to N of (odds that only block i is a 1 pbit)
= sum from i=1 to N of (prob of all zeros)*(1/(prob of 0 pbit))*(prob of 1 pbit)
= (1/N)*sum from i=1 to N of (1/((i-1)/i))*(1/i)
= (1/N)*sum from i=1 to N of 1/(i-1)
=~ log(N)/(N)


The number of blocks an attacker would need to leave money locked in sortition chains, to have 1 bit of influence, if we are limited by evidene period :
(sortition chain evidence period)/(prob that they are winning at the start of the harmonic cycle)/(probability that only one 1-pbit is found during the cycle)*(probability that zero 1-pbits are found)
= (~1000 blocks)/(1/2)/(log(N)/N)*(1/N)
= 2000 /log2(N)



the number of rerolls an attacker would need to do for one bit of influence =
(the odds that after a reroll, the rest are all zeros) =
sum from i=1 to N of i/N =
log2(N)




sum of i=1 to N of (1/i)*(i/N) =
log2(N)







the number of cycles an attacker would need to lock into to have 1 bit of influence =
1/(probability they are winning at the start of the harmonic cycle)/(probability that only one 1-pbit would be found during the cycle)/(probability that the attacker is the one to find the 1-pbit block)
= 1/(1/2)/(log(N)/N)/0.30
=(10*2*N/(3*log(N)))
=(20/3)*(N/log2(N)) cycles

if the harmonic cycle is 128 steps:

= (20/3)*(128/7)
= 120.5 cycles.

The average cycle is log2(cycle) blocks long, so the number of blocks they need to be locked in cycles would be:
(20/3)*N blocks.







Relating security to uncertainty in expiration date
=============

C = cycle length
A = average delay until it finalizes
A ~ C*log2(C)

S = security
S ~ C/log2(C)


so doubling the cycle length means it will take a little longer than twice as long to expire on average, and we will have a little less than twice as much security.









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
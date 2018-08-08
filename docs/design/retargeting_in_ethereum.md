Lets take some ideas from ethereum's retargetting mechanism.

In ethereum, the difficulty of mining your block is dependent on the timestamp.
If the block is made further in the future, then it is less difficult to mine upon.

Nodes sync to the blockchain with the most work, so if 2 blocks are made at the same height, nodes will prefer the one with the earlier timestamp.

We need to show that it is a nash equilibrium to honestly write the time on your block:
- it is already more profitable to use the latest timestamp possible, so we only need to show that it is less profitable to use a timestamp from the future.

"it is less profitable to use a timestamp from the future" == "it is a nash equilibrium for mining pools to refuse to build on a block until that block's timestamp is in the past"

* Block N has a timestamp S seconds in the future.
* Block N - 1 has a timestamp in the past.
* lower difficulty constant > 1.
* probability that no one finds a block on N-1 until the timestamp in N is in the past.

(Expected profit of building on N) = (expected profit of building on N-1)*(lower difficulty constant)*(probability that no one finds a block on N-1 until the timestamp in N is valid)

(profit N-1) / (profit N) # if this is >1, then it doesn't work.
= 1/((lower difficulty constant) * (probability that no one finds a block on N-1 until the timestamp is valid)

1 > 1/((lower diff) * (probability))

((lower diff) * (prob)) > 1
prob > 1/(lower diff constant)

=> it is less profitable to use a timestamp from the future only if (probability that no one finds a block on N-1 until the timestamp in N is valid) is > (ratio of how difficult mining on N-1 is / how difficulty mining on N is)

Lets plug in some examples.
If we cheat by writing a timestamp that is 1 block period in the future, that means there is a 50% chance that no one finds a block until the timestamp is valid.
If the difficulty dropped by more than a factor of 2, this would be insecure.

Another example.
If we cheat by writing a timestamp that is 1/2 of a block period in the future, that means there is about a 75% chance that no one finds a block until the timestamp is valid.
If the difficulty dropped by more than a factor of 4/3, this would be insecure.

Another.
If we write a timestamp that is 2 block periods in the future, that means there is a bout a 25% chance that no one finds a block until the timestamp is valid.
If the difficulty dropped by more than a factor of 4, this would be insecure.

The security threshold of this mechanism is very wide, and we can let the difficult change very quickly without risking incentivizing invalid timestamps.




Things to prove:
* how secure are we against oscillations where miners get turned on/off, or miners switch between blockchains?

Decisions to make:
* ethereum uses 10 seconds as their base time-unit. This is how often the mining pool has to update their timestamp.
* maybe the base time-unit should be in constant ratio with the block time?

Code to write:
* potential_block needs to update at the optimal moments so that the mining pool doesn't waste effort.
* the top of the headers needs to automatically change if the timestamp of a header that is more difficult than the current top header expires.




Proposal 1:
We can track of the exponentially weighted average blocktime = EB
If EB < LT, then the difficulty decreases. (and increase EB to what if would have been if difficulty was lower).
If EB > UT, then the difficulty increases.


proposal 2:
if the block time is less than target, make the difficulty slightly higher. if it is greater than the target, then make the difficulty slightly lower.





For cryptocurrency code to be secure, not only does it need to be impossible to cheat, the default strategy programmed into the full node needs to be a nash equilibrium.
It needs to be impossible to earn a profit by modifying any part of the full node consensus mechanism.

Currently if you are using geth as a mining pool, it is programmed not to build on a block unless that block's timestamp occurred before 15 seconds in the future.

It seems to me that this is similar to the "guess 2/3rds of the median" game, except the mining pools are trying to guess this value:
median of everyone else's choice - (D seconds)
D is a number of seconds such that the probability of the ethereum network finding a block in that period E,
(1-E)*(how much more profitable the lower difficulty block would be) = 1
If a block has a timestamp before (15 + D) seconds in the future, it is more profitable to mine on this block than it's parent.

D is a pure function of:
* how much the difficulty will change by depending on if we mine on one block or the other.
*  the block period.

So every mining pool can calculate the same D.
They are all trying to guess (median of everyone else's guesses - D).
It is my understanding that the expected behavior in a game like this is for everyone to backtrack their clocks more and more each round, exponentially.

The fundamental issue is that Ethereum changes the mining difficulty in 10 seconds intervals. So it is a step function, if the timestamp changes by a single micro second, it changes the profitability of mining on that block.

The step function shape is causing the game of guess 2/3rds of the median.
If we use a continuous function instead, and make sure the slope never exceeds a certain maximum, then we can avoid this problem.

But, using a continuous function to determine the difficulty of mining has a different problem. This means that the mining pool is incentivized to update the problem being worked on as frequently as possible, because a later timestamp is always more profitable.
This rewards certain miners over others, and would be bad.
Ethereum's guarantee that a timestamp is accurate is heavily dependent on blocks with later timestamps being more profitable to mine.
Currently we take the median of a large number of blocks, and trust that >50% of hashpower is providing good timestamps.
Using medians like this limits how quickly we can adapt to changing hashpower. It takes too long to do medians.
This is why I am researching Ethereum's faster method.
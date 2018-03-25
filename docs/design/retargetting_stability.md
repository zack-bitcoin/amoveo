consider how much hashpower the network is willing to supply at a given price.
Suppose it is of the form hash_power(p) = C/p. for some constant C. Could this be stable?

First we calculate the block period based on the hash power:
block_period(p) = hash_power(p) / p = C/(p*p)


consider examples: if the price doubles, what happens to the block period?
```
new_period / old_period =
block_period(2p) / block_period(p) =
(C/(4*p*p)) / (C / (p*p)) =
4.
```
So if the price should double, then the block period would increase by a factor of 4.

So if hash_power(p) is truly of the form C/p, then when we want to retarget, we should do it by a different ratio.
Instead of (New Price) = (OldPrice) * 600 / (block period)
It should be (New Price) = (Old Price) * sqrt(600 / (block period))



Now we consider if hash_power(price) = C, for some constant C. This would be the case if people mine at the same rate regardless of the price.

block_period(p) = hash_power(p) / p = C/p

so if the price doubles, what happens to the block period?
```
new_period / old_period =
C/(2p) / (C / p) = 2.
```
so the block perdio would double.
So if hash_power(p) is of the form C, then our current retargetting formula is ideal.

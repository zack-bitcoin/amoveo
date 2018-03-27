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




Now we consider if hash_power(price) = max(0, C - price), for some constant C. This case is more similar to reality in that there is an upper limit above which practically no one mines.

block_period(p) = hash_power(p) / 0 = max(0, (C - p) / p)

so if the price starts at C/10 and doubles, what happens?
```
new_period / old_period =
(9C/(2*p*10)) / (8C/(p*10)) = 9/4.
```
therefore, the period would increase by about 125%


what if the price starts at 2C/3 and doubles?
```
(1C/(3*2*p)) / (0/p) = infinity.
```

In this case the ideal retargetting formula would need to account for how far away C is, since we should never go bigger than C.



Now we consider if hash_power(price) = {price < L -> C * 5; price > L -> C}. Lets consider the case where price = L-epsilon as epsilon tends to zero, and it doubles.

```
new_period / old_period =
(C*5/(2*p)) / (C/p) = 2.5.
```
so the period would increase by a factor of 2.5

It would oscilate the same amount up and down.
It seems like the miners who are able to participate on the more difficult side of things are incentivized to do enough extra work to keep the difficulty high, and stop the oscilation. 
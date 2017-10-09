You could implement some different difficulty retargetting algorithms, and then run tests to see how they perform under stress.

Like if the hashrate drops by a factor of 10, how long till the difficulty fixes itself?
Or if the hashrate increases by a factor of 10?
Or what if a miner mines in an oscillatory way, how much extra profit can they take?

I did a little work towards this here: https://github.com/zack-bitcoin/zack-retargeting

and here: https://github.com/zack-bitcoin/basiccoin


For Amoveo I copied the retargetting algorithm used in bitcoin.
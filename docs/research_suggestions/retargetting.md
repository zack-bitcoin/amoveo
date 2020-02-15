WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//research_suggestions/retargetting.md)

You could implement some different difficulty retargetting algorithms, and then run tests to see how they perform under stress.

Like if the hashrate drops by a factor of 10, how long till the difficulty fixes itself?
Or if the hashrate increases by a factor of 10?
Or what if a miner mines in an oscillatory way, how much extra profit can they take?

I did a little work towards this here: https://github.com/zack-bitcoin/zack-retargeting

and here: https://github.com/zack-bitcoin/basiccoin


For Amoveo I copied the retargetting algorithm used in bitcoin.

How profitable can this be
==========

The value of land in a dense city like Paris can be $10k per square meter in an apartment building.

Undeveloped land can cost <$500 per hectare, or $0.05 per square meter of land.

If a 10 story building is put on top, and the building is a part of a big city, that $0.05 of land becomes worth $100 000. Even in a highly regulated market where buildings are more expensive than they need to be, it only costs around $25 000 to build the building. So the land itself is $75 000 per square meter.
A 1.5 million % increase in value of the land.

What are we doing
=========

What makes a city valuable is that lots of people are invested at the same time.
If we can coordinate many people's investmets and decisions, then we can convert cheap land into a valuable city and everyone involved will be richer.

We can use a blockchain to record ownership of land in the city before we have decided where to put the city.

We can use futarchy to make any decisions that need to be made, like where to put the city.


Legal and Tax advantages
========

When Amazon was looking for a city to establish it's headquarters, cities would compete for Amazon by giving special privileges. They offered tax and legal advantages.

Many countries would be very interested in having a new large city established on their undeveloped land. Countries would compete with each other to offer the best legal and tax advantages to convince us to put the city in their land.

This is a strategy already in practice by Mennonites for example. When they established their new cities, they make an agreement with the local government so that they could do schooling for their children differently. Mennonite communities are great for the economy, they create many job opportunities, so countries compete by offering this legal advantage.

Potentially we could make the city a free trade zone, and give automatic national residency to everyone who owns above some minimum amount of square meters in the city.

Database for storing land ownership
=======
The current merkel trie database is a trie where each node points to up to 16 other nodes.
We want there to be a maximum depth for the merkel trie, and we don't want to have to re-balance the trie.

tx types: divide a square, combine a square, and spend a square.

one transaction type that divides a square into 16 smaller squares, in a 4x4 grid. Then any of the 16 squares can be transfered individually.
It costs a fee to divide into 16 because it is using up more space.

If one person buys up all 16 squares, they can make a different kind of tx to combine them into 1 bigger square, and they receive some sort of compensation for recovering space in the blockchain.

Each square records:
who owns it.






Open questions
=======

What is the best kind of database for the blockchain to store records of property ownership?
Perhaps a merkel tree where locations that are near to each other spacially also share the same branch of the merkel tree.
That way if you wanted to prove a bunch of information about one neighborhood, the merkel proofs would share a lot of data in common, so you would need less data.

Should we release blocks of the city as the reward for finding new POW blocks?
A block reward block would be great.

Instead of "real estate", what is this called? "fake estate"? "digital estate"?


##### using governance.


We need to make futarchy markets before we update the governance variable, that way the community can come to consensus about what updates should be made.

The oracle is a reporting mechanism, it isn't made for coming to consensus. If you try to use it to come to consensus, it is like a game of chicken, and is bad for the network.


For example, if you want to make governance oracle to change the block reward size, you first need to make a question oracle of something like `Q="we increase the block reward by 20%";P="the market cap of VEO exceeds $5 million";(P and Q) or (!P and !Q)`
Then run a market in the channels on this oracle. If the market strongly values shares of True more than they value shares of False, then we can consider changing a governance variable.
IDEX sidechains review
========

[Here is a blog post from IDEX describing their sidechain plan](https://blog.idex.io/all-posts/o2-rollup-overview)

It looks like the IDEX team will be operating a trusted sidechain to store all the trades in their decentralized exchange.
The problem with this strategy is that the IDEX team can choose the order to do the trades. They can front run everyone.
Front-running is just one example of how it can break.

[Here is a video about why markets that are vulnerable to front-running attacks are insecure](https://youtu.be/mAtD0ba-hXU)

The fact that they refer to this as "optimistic roll up" makes me consider it a scam.
In optimistic roll up as defined by this paper https://arxiv.org/pdf/1904.06441.pdf anyone who mines a main chain block can include some data to add a block to the sidechain.
There is a safety deposit paid when you add a block to the sidechain.
Optimistic roll up is decentralized, anyone who is participating is the same as everyone else.

The IDEX plan in this blog post is completely different. Their team is the only ones who can add blocks to the sidechain. This creates a centralized failure mode.



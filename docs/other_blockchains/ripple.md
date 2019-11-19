Ripple
=========

Ripple is protocol for a centralized and trustful currency-inspired tool.

This is the white paper for ripple https://arxiv.org/pdf/1802.07242.pdf

UNL
=======

With Ripple, in order to know which fork is legitimate, you need to maintain a record of which nodes are on the trusted node list (called UNL).

According to section 4 of the Ripple white paper, a pair of nodes can only agree on which ripple transactions are legitimate if they are both using UNL's that are sufficiently similar.


Going offline is insecure and trustful
==========

Bitcoin PoW is trust-free technology.
You never have to trust someone to run a node.
With PoW there is always exactly one fork that has the most work done, and you can verify which fork has the most work even if that work was done while you were offline.

With Ripple, in order to know which fork is legitimate, you need to maintain a record of which nodes are on the trusted node list (called UNL). In order to maintain this list, you need to know if any ripple nodes fail to participate in the ripple protocol within the correct time period, that way you can know they should be removed from the trusted list. The only way to know if a ripple node took too long to sign a message is if you are online 24/7 watching to see who signs what messages at what times.

If you were offline for any amount of time and then you come online, then it is not possible for you to know which nodes on the UNL took too long to sign messages, so you can't know which nodes should be removed from the UNL, so you can't know which side of a fork is the legitimate Ripple chain, so you can't know if a payment sent to you has any value. The payment could be on the illegitimate side of the fork, and you have no way of knowing.





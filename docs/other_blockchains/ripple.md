Ripple
=========

Ripple is protocol for a centralized and trustful currency-inspired tool.

This is the white paper for ripple https://arxiv.org/pdf/1802.07242.pdf

UNL
=======

With Ripple, in order to know which side of a consensus-fork is legitimate, you need to maintain a record of which nodes are on the trusted node list (called UNL).

According to section 4 of the Ripple white paper, a pair of nodes can only agree on which ripple transactions are legitimate if they are both using UNL's that are sufficiently similar.

If you don't know which nodes belong on the UNL, then you can't know if a ripple transaction is valuable or if it is on a worthless fork.

5-second block time
=========

Ripple has a 5-second block time. So if any nodes fail to participate in the protocol fast enough for 5-second blocks, then they need to be dropped from the UNL so that they don't slow us down.

Only users that are online will have any evidence about which nodes failed to sign messages within the 5-second period and which nodes are too slow and need to be dropped from the UNL.

So, the only way to maintain a proper UNL is if you were operating a ripple node that was online 24/7 ever since the first Ripple block in 2012. If you ever went offline for more than 5 seconds, then you don't know what the proper UNL is.


Going offline in ripple is insecure and trustful
==========

Bitcoin PoW is trust-free technology.
With PoW there is always exactly one fork that has the most work done, and you can verify which fork has the most work even if that work was done while you were offline.

In Ripple, if there was ever a 5 second period between now and 2012 when you were not operating a ripple node, then you can't know which nodes belong on the UNL.
If you don't know which nodes belong on the UNL, then you can't know which side of a fork is legitimate.
If you don't know which side of the fork is legitimate, then you can't know if a transaction sending coins to you has value or not.




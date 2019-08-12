Zano Review
=======

Here is the Zano white paper: https://zano.org/downloads/zano_wp.pdf

Here is a general proof that PoW/PoS hybrid protocols are impossible: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/pow_pos_hybrid.md


Fork Choice Rule
========

in zano the fork choice rule isn't a cumulative weight function looking at a single history.
It is a function that compares 2 alternative histories, and chooses one.
It is a function with 4 inputs.
```
weight(P1, P2, H1, H2) = (P1 + H1) * P1*H1/(P2*H2)
```

multiplying it all out:
```
weight(P1, P2, H1, H2) = (P1*P1*H1 + P1*H1*H1)/(P2*H2)
```

In practice, we are always comparing the weigh of one side against the other.

```
weight2 = (P2*P2*H2 + P2*H2*H2)/(P1*H1)
```

if side 1 wins, that means ->
```
(P1 + H1) * P1*H1 / (P2*H2) > (P2 + H2) * P2 * H2 / (P1 * H1)
```
simplifies to ->
```
(P1 + H1) * (P1*H1)^2 > (P2 + H2) * (P2*H2)^2
```


So lets suppose an attacker has 2/3rds control of the stake, and calculate how much hashpower they would need to take control. Fork2 is the attacker's side.

`P2 = 2*P1`

To keep it simple, the non-attacker's side of the fork has normal amounts of participation and hashrate. P1 = 1, H1 = 1

```
(1 + 1) * (1*1)^2 = (2+H2) * (2*H2)^2
-> 2 = 8*H2^2 + 4 * H2^3
-> 1 = 2*H2^2(2 + H2)
```

Since H2 is positive, there is only one solution. H2 = 0.451 (I used wolfram alpha to calculate this)
Which means an attacker with control of 2x as much stake as the main chain would only need 45% control of the hashrate to take control of the blockchain.

45% < 51%, so it is cheaper to attack this protocol vs standard PoW.


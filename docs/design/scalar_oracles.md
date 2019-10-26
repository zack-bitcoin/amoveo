Effective use of scalar oracles
==============

An oracle is a mechanism inside a blockchain to help the blockchain learn facts. Like we can teach the blockchain what the price of BTC in USD was at a particular moment in time, that way the blockchain can enforce smart contracts using this information.

If you want to make a stablecoin by betting on the price of VEO in BTC at a particular moment in time, then you may need to use an oracle to securely import that price data into the blockchain.

Scalar oracles generate a value that can be represented in binary bits, like this `0b01011010`

if the oracle measures `0b00000000` that is the lower  margin. `0b11111111` is the upper margin

if the price is 1/2, that is `0b10000000`, which has the least accuracy of anything we could measure. 0 bits of accuracy.
Because `0b01111111` is practically the same


but if the price is 1/8th the margin, that is `0b00100000`, which is nearly the same as `0b00011111`.

notice that the first 2 bits in each of these are the same.

so the first 2 bits will still be correct, even if the rest is a mess. so if the price is 1/8th the margin, we have 2 bits of accuracy. 

```
total loss due to uncertainty =
sum from i=0 to N=(number of bits) of  [
   (probability of uncertainty in bit i)
] =
sum form i=0 to N of ((2^i)/(2^N))) =
((1/2)^N) * sum from i=0 to N of (2)^i =
N*(N+1)/(2^(N+1))
```

So if we use 11 bits to encode the value, in the worst case, this like a 3.2% fee to use stablecoin contracts or any scalar derivative contrat.


Solution
==========

The layer 2 smart contract can reference several different oracles that have not been published on-chain yet, and give higher nonce priority to whoever is willing to use a larger fraction of the oracles to settle their contract.
For a number of oracles to sample N, there is some deterministic function to tell you which of the oracles you need to use.
Using a larger fraction of the oracles means you need to pay more fees to publish more new_oracle txs. So the cost of (these extra fees) - (extra fees the attacker would need to pay) becomes an upper limit on how much the attacker can steal.
The difference is the cost of making 1 more scalar oracle.

If we have N bits of accuracy about the outcome, then 

```
b = amount being bet,
c = cost to make 1 more scalar oracle
D = lesser of b or c
the loss due to uncertainty = D*N*(N+1)/(2^(N+1))
```

So if this is inside of a sortitoin chain of 1000 block rewards, and the fee to make a new scalar oracle is 0.1 block rewards, then that means this failure mode is at worst, like a 0.00032% fee paid to create a new sortition chain.

[more about sortition chains here](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md)

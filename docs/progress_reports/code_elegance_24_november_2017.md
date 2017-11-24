Aeternity made a strong complaint about Amoveo recently. They claim that Aeternity "succeeded in rebuilding æternity’s testnet from scratch in one fourth of the size of the previous code." https://blog.aeternity.com/announcing-new-development-timeline-and-team-restructuring-11097435ea32

It sounds to me like Aeternity is saying they were able to make my software more efficient and smaller.
This is a big surprise to me. When I had been working with this team, they only managed to make software excessively verbose. When I look at the Aeternity code base, it seems like they use 4 or 5 times as many lines as I do.

So I thought I would count all the lines, and compare. 

## Aeternity 

The Aeternity code base inside the apps directory of epoch excluding aecore on github is currently 14990 lines, and aecore, the consensus part of Aeternity is 6492 lines.
So in total, Aeternity is 21482 lines of code.

Aeternity recently announced that it is possible to spend tokens on Aeternity, so they have 1 transaction type. Their testnet is not yet live.
Aeternity does not yet have sharding, or channels, or oracles, or markets, or light nodes.

## Amoveo

The Amoveo code base inside the apps directory, besides ae_core on github is currently 8644 lines, and ae_core, the consensus part of amoveo is 7752 lines.
These libraries were written to be used by amoveo: 623 lines for a forth-like virtual machine, 370 lines for proof of work cryptography, 1271 lines for merkle trees, 343 lines for the encryption library

So including in-house dependencies, Amoveo is 19003 lines. This means Amoveo is 88.5% the size of Aeternity.
Amoveo has a full node and light node that can connect to a live testnet.
Amoveo supports 16 working transaction types. They are for accounts, oracles, and turing complete state channels. Amoveo has sharding, turing complete state channels, oracles, off-chain trustless markets in the lightning network, and light nodes right now. You can try it today.


When you look at them both side by side, it is surprising how little Aeternity can accomplish with such a large number of lines of code.
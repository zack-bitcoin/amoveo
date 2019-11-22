Veriblock offloading review
========

Offloading transactions to the security providing blockchain is an area of research currently being developed by the Veriblock team.

Veriblock's current sidechain strategy is only protecting against attacks that re-write history. It does not offer any security against censorship attacks. The goal of the offloading transactions plan is to add security against censorship to Veriblock sidechains.

How offloading works
========

Since the main chain is containing headers for all the side-chains, it is possible for the main chain to verify a merkel proof to know about account balances in the sidechains.

So, if a transaction is being censored in a side chain, you can pay to have it written to the main chain.

All the side chains have a rule that if the main chain is including transaction for their sidechain, they need to include all that data into their side-chain blocks for the blocks to be valid.

The problem with offloading
==========

If many of the sidechains all had censorship issues simultaniously, there will not be enough space in main-chain blocks to include all the transactions that are being censored.

So the offloading strategy does not work to prevent censorship attacks.
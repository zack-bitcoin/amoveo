Connecting blockchains together
==========

There are 3 popular strategies for building applications that use multiple blockchains.

1) Blockchain A is a dependency of blockchain B. In this case, every full node of B also needs to run a full node of A.

2) Blockchain A's light node is a dependency of blockchain B. In this case, every full node of B also needs to run a light node of A.

3) Cross chain atomic swaps and hashlocking and channels. In this case, both blockchains are enforcing the same deterministic contract, which will have the same result on both. The relationship between blockchains in this case is the same as the relationship between channels in Amoveo. So any blockchain that supports this type of relationship can probably interact in the same way with Amoveo contracts.


(1) is best if you want to work with bitcoin, because bitcoin's light nodes aren't good enough for (2).

(2) is best if you want your blockchain to use another blockchain's oracle, or if you want to know something specific about the other blockchain's state, like the current mining difficulty.

(3) is best if you want to transfer money or synthetic assets, or if you want to participate in markets.


Why aren't bitcoin's light nodes good enough?

2 works really well with Amoveo, because Amoveo light nodes are so light
light nodes of bitcoin are really bad because making a proof of an account's balance takes a long time to generate.
bitcoin's UTXO isn't optimized for light nodes at all.
To make a proof of an address's balance, the full node has to rescan every transaction from every block, which takes hours.
If verifying a block of B requires rescanning blockchain A, then it would take hours to verify a block.
Maybe at the end of hours of work you will find out that the block was invalid.
If miners waste hours, just to know which block to mine on next, this is a fatal problem.
so strategy (2) doesn't work with bitcoin right now.

Cosmos is a cool project, when they want to connect Bitcoin to blockchain A, they do a type 1 connection from Bitcoin to C, then a type 2 connection from A to C. https://cosmos.network/
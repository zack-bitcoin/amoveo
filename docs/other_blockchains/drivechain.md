Drivechain
=====

Drivechain is a project with the goal of allowing for multiple different blockchains to be secured by the PoW consensus on a single blockchain. It is a goal of drivechain that bitcoin miners should not be required to think about or involve themselves with the side-chains.


Double-Spend Attacks. Finality issues.
=======

Mainchain miners don't care if the side-chain block they are validating will get orphaned or not. They will get paid the fees either way.

If the mainchain miners frequently orphan blocks, this can indirectly harm them because it makes the entire network worse. But, this cost to the mainchain miner is on the order of (percentage of hashpower they have.) As the hashpower gets more distributed, they are more and more willing to allow sidechain blocks to get orphaned.
As long as the side-block that will cause the orphaning to occur is paying a higher fee than the cost to the miner of orphaning the previous block, then the miner is willing to verify it.
Even though the benefit to the miner of doing this is far smaller than the cost to the network of the side block getting orphaned. It is tragedy of the commons.

side-chain orphans are different from main-chain orphans, because the side-chain blocks are paid for in liquid currency, while main-chain blocks are paid for in illiquid hashpower.

If you try to buy up 51% of hashpower, it keeps getting more expensive the more you buy. 
If you have 2x more hashpower than the entire network, you can still only rewrite 1 hour per hour.
So if you had 2x more hashpower for a period of 10 hours, you could rewrite 10 hours of pow history. So to rewrite 10 hours of history, you need to spend as much as 20 hours of (block rewards).

With proof-of-burn, and with drive-chain side-chains, you only need to be willing to accept a slightly smaller reward for creating 10 hours of blocks in comparison to whoever created them the previous time.

undoing side-chain blocks in drivechain is akin to undoing blocks in a proof-of-burn blockchain. Anyone who is willing to accept a smaller reward for side-block creation can rewrite those side-blocks.

This means drive-chain sidechains will have very slow finality, it is not clear if a tx can ever truly be finalized.


Freeze Attacks
=======

If you want to cause a delay in bitcoin, you need to pay more than the cost of (all the fees) + (all the block rewards) for the period in question, and the price of fees will keep getting higher, because the scarcity will drive demand. So you are paying ((block reward) + ((extra high fee price) * (as many tx as can fit in a block))) * (number of blocks during the attack period).

If you want to cause a delay in a drivechain sidechain, you just need to be willing to receive a slightly smaller reward than whoever had created the blocks in that history the previous time.

Since it is cheap to cause the sidechain to stop processing txs, and an attacker could profit from freezing a sidechain by making bets in other markets, this means drivechain sidechains are trust level 4. https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md


Sidechain block creation griefing
===========

If someone spends $99 to earn $100 and create a block, and I spend $101 to earn $100 and create the same block to cause their block to get orphaned, then that means I lost only $1, but I caused someone else to lose $100.
Since it is possible to destroy more of someone else's value in comparison to the cost of the attack, that means that Drivechain sidechains are trust level 3 or worse. https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md


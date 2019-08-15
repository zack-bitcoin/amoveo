Drivechain Review
=====
Draft # 3

Drivechain is a project with the goal of allowing for multiple different blockchains to be secured by the PoW consensus on a single blockchain. It is a goal of drivechain that bitcoin miners should not be required to think about or involve themselves with the side-chains.

Abstract
=======

The goal of this paper is to that it is not possible to use drivechain as it is currently designed to secure any sort of side-chain consensus state. It will always be cheap for an attacker to harass the users of a side-chain to the point where the side-chain is unusable. The PoW security from bitcoin is not properly securing the side-chain.

The critique in this paper has not been responded to before
===========

The critique I am making here is neither (1) nor (2) from this paper: http://www.drivechain.info/peer-review/peer-review-new/

* I am not describing a way for the miner to steal sidechain funds.
* I am not describing a way that this could cause pool overhead or txn-censorship.

In this paper http://www.truthcoin.info/blog/blind-merged-mining/ in the section "Problem", Paul describes how an attacker could spam invalid block data to prevent a side:chain from progressing, and why this is not a security issue, because the main chain miners could temporarily upgrade to be side chain full nodes to know which version is valid.

This problem is different from the critique I am making in this paper. The "Problem" Paul fixed is how to prevent side:chain hard update. The problem I am describing is somewhat related. In my critique, the attacker is making a side:soft update.

Paul's solution to merely upgrading miners to side:full-node status works great for preventing side:hard updates, but it does not solve the soft-fork problem, because both side of the soft-fork look valid to a side:full-node.

Description of the broken mechanism
============

The part of drivechain that does not work is the blind merged mining.
Here it is described by Paul: http://www.truthcoin.info/blog/blind-merged-mining/

I will describe it in my own words:

In order for a side:block to be valid, it's hash needs to be recorded into a main:block.
So sidechain block creators need to pay the mainchain miner to include this hash in their block.
Since anyone can create the side:block, the side:block creator needs to give practically all of the fees from that side:block as a bribe to the mainchain miner.

In the "handling reorganizations" section, Paul explains about the fork choice rule for the side chain. The version of the side:chain fork that gets more confirmation hashes recorded onto the main:chain wins.

This means that the side of a side:chain fork that gets more confirmations is the valid version of history.

Any fees the side-chain block creators had paid to let their side-blocks get created, they do not get a refund if their side:block is orphaned.
If the side:block is orphaned, they do not receive any tx fees.

So it is very expensive for the sidechain block creator if their block gets orphaned.

Why it is cheap to cause side-blocks to get orphaned
===================

Mainchain miners don't care if the side:block they are validating will get orphaned or not. They will get paid the fees either way.

If the mainchain miners frequently allow side:blocks to get orphaned, this can indirectly harm them because it makes the entire network worse. But, this cost to the mainchain miner is on the order of (percentage of hashpower they have.) As the hashpower gets more distributed, they are more and more willing to allow side:blocks to get orphaned.

As long as the side-block that will cause the orphaning to occur is paying a higher fee than the cost to the miner of orphaning the previous side:block, then the miner is willing to verify it.

Even though the benefit to the miner of doing this is far smaller than the cost to the network of the side block getting orphaned. It is tragedy of the commons.


Double-Spend Attacks. Finality issues.
=======

side-chain orphans are different from main-chain orphans, because the side-chain blocks are paid for in liquid currency, while main-chain blocks are paid for in illiquid hashpower.

If you try to buy up 51% of hashpower, it keeps getting more expensive the more you buy. 
If you have 2x more hashpower than the entire network, you can still only rewrite 1 hour per hour.
So if you had 2x more hashpower for a period of 10 hours, you could rewrite 10 hours of pow history. So to rewrite 10 hours of history, you need to spend as much as 20 hours of (block rewards).

With proof-of-burn, and with drive-chain side-chains, you only need to be willing to accept a slightly smaller reward for creating 10 hours of blocks in comparison to whoever created them the previous time.

undoing side-chain blocks in drivechain is akin to undoing blocks in a proof-of-burn blockchain. Anyone who is willing to accept a smaller reward for side-block creation can rewrite those side-blocks.

This means drive-chain sidechains will have very slow finality, it is not clear if a tx can ever truly be finalized.


Freeze Attacks
=======

If you want to cause a delay in a drivechain sidechain, you just need to be willing to receive a slightly smaller reward than whoever had created the blocks in that history the previous time.

Since it is cheap to cause the sidechain to stop processing txs, and an attacker could profit from freezing a sidechain by making bets in other markets, this means drivechain sidechains are trust level 4. https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md

If you want to cause a delay in bitcoin, you need to pay more than the cost of (all the fees) + (all the block rewards) for the period in question, and the price of fees will keep getting higher, because the scarcity will drive demand. So you are paying ((block reward) + ((extra high fee price) * (as many tx as can fit in a block))) * (number of blocks during the attack period).

Sidechain block creation griefing
===========

If someone spends $99 to earn $100 and create a block, and I spend $101 to earn $100 and create the same block to cause their block to get orphaned, then that means I lost only $1, but I caused someone else to lose $100.
Since it is possible to destroy more of someone else's value in comparison to the cost of the attack, that means that Drivechain sidechains are trust level 3 or worse. https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md




Paul's about bitcoin without block rewards
==========

Paul admits that a basic assumption of his design for drivechain, is that drivechain can only work if it is possible that bitcoin can be secure with no block rewards.

Paul from Drivechain Telegram- "one of my arguments for drivechain is that we can use it to learn about the behavior of blockchains that lack a block reward."

There is no evidence that Bitcoin can be secure without block rewards, and there is a lot of evidence that it will not be secure: http://randomwalker.info/publications/mining_CCS.pdf

From a game theory perspective, it is in the interest of the Bitcoin community to pretend that the 21 million coin limit is possible to enforce, even if in reality it is not possible.
The 21 million limit is a nice story that makes people more interested in Bitcoin.

If the 21 limit is impossible, it will be many years until Bitcoin is forced to do a patch to break the 21 million limit. Until that day comes, it is in the bitcoin community's self-interest to maintain the lie about the 21 million limit.

So, the fact that the 21 million limit is still in the bitcoin software tells us nothing about whether it is possible for bitcoin to be secure without block rewards.

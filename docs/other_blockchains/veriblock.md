Veriblock review
=========

Veriblock is a kind of side-chain project.
Veriblock is not trying to move money back and forth between the sidechains.
They just want the sidechains to inheret the PoW security from the mainchain.

Veriblock aims to enable a security inheriting blockchain to inherit the complete proof-of-work security of a security providing blockchain.

Here is the old version of the veriblock white paper: https://www.veriblock.org/wp-content/uploads/2018/03/PoP-White-Paper.pdf

Here is the new version of the veriblock white paper: https://mirror1.veriblock.org/Proof-of-Proof_and_VeriBlock_Blockchain_Protocol_Consensus_Algorithm_and_Economic_Incentivization_v1.0.pdf

Two veriblock designs
=========

The veriblock white paper offers many choices on how to optimize your veriblock sidechain for your situation.
One choice you need to make is whether you will embed the entire veriblock header into bitcoin transactions, or whether you will only embed a hash of the veriblock header.

Bribery vulnerabilities
=========

If we use the version of veriblock where the entire veriblock header is embedded in bitcoin transactions, then it has bribery vulnerabilities.

The attacker could bribe a bitcoin miner to include the attacker's veriblock header in the block, and to not include anyone else's veriblock headers. This way, the attacker's side of the veriblock fork would get more confirmations.

The attacker doesn't need to undo history for this attack to succeed.
By merely controlling which veriblock headers are added to the longest chain, the attacker can perform arbitrary soft fork updates to the rules of the veriblock sidechain.
So the attacker could steal the money on the sidechain.

The cost of this attack is very cheap. The attacker could bribe 1% of bitcoin miners to censor competing veriblock headers.
If 99% of bitcoin miners include headers for both sides of a veriblock fork, and 1% of bitcoin miners only include a header for the attacker's side of the veriblock fork, then the attackers side will win.

Data withholding vulnerabilities
========

If we go with the version of Veriblock that embeds just the hash, then Veriblock sidechains are vulnerable to data withholding attacks.

An attacker could build a veriblock fork that is longer without revealing to anyone else the contents of the blocks, or the fact that the veriblock sidechain has an active fork.

Since only hashes are embedded and not full headers, we can't know wether the veriblock sidechain has forked or not.

Then the attacker can double-spend all their transactions on the uncensored side of the fork by revealing their side of the fork to undo some history.

So the attacker could steal the money on the sidechain.

Conclusion
=======

Regardless of how you configure your veriblock sidechain, it is not secure.

Veriblock sidechains are a similar design as drivechain. They have the same vulnerabilities if we embed the entire header. https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/drivechain.md

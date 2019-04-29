Frequently Asked Questions
==========

* Sybil control
  There are no identities in Amoveo. Pretending to be two people doesn't give you any more privileges than being one person.
  
* Censorship resistance
  Nakamoto proof of work
   
* Scalability
  Amoveo uses [channels](channels.md) for scalability.

* Access control
  Same as bitcoin, you use a private key to sign transactions to be recorded onto the blockchain.

* Privacy
  Same as bitcoin, the accounts are all psuedononymous. 

* Auditability
  Anyone can run the open source code. The definitions of every transaction type are short enough to be quickly verified by any programmer. Besides running transaction verifications, we have an additional check to verify that no new veo was created during the processing of any block. It is only like 30 lines, and just verifying those 30 lets you know that veo can never be counterfeited.

* Upgrade/fork mechanisms

The amoveo community uses futarchy for controversial updates.

* Proof of correctness for consensus

Amoveo uses Nakamoto consensus, the same as bitcoin.

* Data availability (if sharding)

Smart contracts are stored off-chain in the channels, so there is never an availability issue for them.

On-chain consensus state is all in merkel trees.

In Amoveo every block contains all the merkel proofs for all the data needed to verify that block. This is convenient because it means you don't have to read anything from the hard drive to verify the block, so we can verify blocks in parallel.

If you have the merkel proofs for the data needed to verify your tx, then you can publish that tx to any mining pool running on any shard.

You can find out the merkel proof for the data you care about by looking up recent blocks for that shard, and reading the merkel proofs that are stored with each block. So even if the mining pools refuse to send you and merkel data for their shard, you or anyone else can re-compute any part of that data.

* Asynchronicity (orphans)

    Amoveo uses Nakamoto consensus, the same as Bitcoin.

* Known attack vectors (e.g. 51%)

    an attacker with >50% hashpower can censor anything he wants, and rewrite any history

* Economic incentives

    
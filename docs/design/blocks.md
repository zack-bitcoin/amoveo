# Block

The following is a simplified model of the data structures related to
the block.

## Block

It includes:
* [Block header](#block-header);
* Transactions;
* Partial representation of state Merkle trees (e.g. accounts,
  channels) before application of transactions;
  * Each of such state Merkle tree is represented as:
    * Proof and datum of each leaf affected by at least one
      transaction in the block;
    * Hash of the root.
* Hash of the root of each of the state Merkle trees (e.g. accounts,
  channels) after application of transactions.

## Block header

It includes:
* Hash of previous block header;
* Height;
* Approximate timestamp of mining;
* Difficulty;
* Hash summarizing transactions (in block);
* Hash summarizing all state Merkle trees (e.g. accounts, channels) as
  after application of transactions;
* Proof of work.

### Examples

#### Example - Block with only one transaction

The aim of this example is explicating that each block contains only
the leaves affected by the transactions in the block.  This also means
that each block does not necessarily contain proof and datum of every
leaf of each state Merkle tree before application of transactions.

For example, an oversimplified block B with only one transaction could
include:
* One transaction T, decreasing balance of account A1 by 5 and
  increasing balance of account A2 by 5;
* Hash of the root of the accounts Merkle tree M1 before application
  of transaction T;
* Proof and datum for each of the following leaves of the accounts
  Merkle tree M1:
  * Account A1 with balance 10;
  * Account A2 with balance 20.
* Hash of the root of the accounts Merkle tree M2 after application of
  transaction T.  In tree M2, account A1 is present with balance 5 and
  account A2 with balance 25.
  * The datum of any leaves of the accounts tree M2 distinct from A1
    and A2 cannot be inferred only from the information in block B;
    neither can the presence or the proof of leaves of the accounts
    tree M2 distinct from A1 and A2 be inferred only from the
    information in block B.

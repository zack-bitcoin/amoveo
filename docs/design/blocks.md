# Block

The following describes the block-related data structures as exchanged
among peers in the network.  It also maps, where possible, items in
such network-exchanged data structures to items in the internal
representation of data structures in the Erlang codebase.

The following describes how things are meant to be, and maps to how
things are in the Erlang codebase.  Items described here and known not
to be reflected yet in the codebase are noted with a *TO-BE* prefix.

## Glossary

["Merkle tree"](https://en.wikipedia.org/wiki/Merkle_tree).

"Leaf datum", in the context of a Merkle tree: value of the leaf.

"Leaf proof", in the context of a Merkle tree: list of Merkle tree
nodes from the leaf to the root proving the inclusion of the chosen
leaf datum.

## Block

The block includes:
* [Block header](#block-header);
* Transactions;
  ```erlang
  #block.txs
  ```
* Partial representation of state Merkle trees (e.g. accounts,
  channels) before application of transactions, specifically proof and
  datum of each leaf affected by at least one transaction in the
  block.
  ```erlang
  #block.proofs
  ```
  * Explicating further the description above:
    * Each block does not necessarily contain proof and datum of every
      leaf;
    * The datum of any leaves not affected by at least one transaction
      in the block cannot be determined only from the information in
      the block, neither can the presence or the proof of most leaves
      not affected by at least one transaction in the block be
      determined only from the information in the block.
  * *TO-BE The codebase holds a place but does not use such item.  The item shall be used.*

See also [aeternity whitepaper] subsection "II-A.4) Block contents".

## Block header

*TO-BE The codebase always exchanges among peers in the network the block - never the block header.  The block header shall be exchanged among peers in the network on its own.*

The block header includes:
* Hash of previous block header;
  ```erlang
  #header.prev_hash
  #block.prev_hash
  ```
* Height;
  ```erlang
  #header.height
  #block.height
  ```
* Hash summarizing partial representation of state Merkle trees before
  application of transactions (in block) and transactions (in block);
  ```erlang
  #header.txs_proof_hash
  ```
* Hash summarizing all state Merkle trees (e.g. accounts, channels) as
  after application of transactions;
  ```erlang
  #header.trees_hash
  #block.trees_hash
  ```
* Approximate timestamp of mining;
  ```erlang
  #header.time
  #block.time
  ```
* Difficulty;
  ```erlang
  #header.difficulty
  #block.difficulty
  ```
* Proof of work.
  ```erlang
  #header.nonce
  #block.nonce
  ```
  * A proof of work depends on the whole block header and is
    parametrized by the difficulty.

See also [aeternity whitepaper] subsection "II-E.2) Light clients".

## References

[aeternity whitepaper]: https://blockchain.aeternity.com/%C3%A6ternity-blockchain-whitepaper.pdf

## TODO

Check in codebase and describe computation of difficulty. E.g. every 2000th block retargets the difficulty based on the previous 2000 block headers.

Assuming that transactions in block are meant to be able to be applied in arbitrary order on the state Merkle trees, both when making the block header to-be-mined and when verifying the block, explicate constraints on transactions in block. E.g. can a block include for the same account a transaction crediting it and another debiting it - where the debiting transaction would make the account balance negative if applied before the crediting one?

Confirm from the codebase that `#leaf.meta` (as opposed to `#leaf.value`) is not meant to be exchanged on the network as part of the data structures described in this document, then reconsider using the term "value" rather than "datum" for leaves of Merkle trees.

Detail Merkle tree and Merkle proof.

Decide whether to include `#header.accumulative_difficulty` in this description.

Decide whether to include `#header.version` and `#block.version` in this description.

Decide whether to include `#block.comment` in this description.

Confirm exclusion of `#block.prev_hashes` from this description.

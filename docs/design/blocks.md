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


## Block header

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
* Protocol version;
  ```erlang
  #header.version
  #block.version
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

The block header `H` is serialized as per function
`headers:serialize/1`:

```erlang
HB = 32*8,
HtB = 32,
TB = 32,
VB = 16,
DB = 16,
<<(H#header.prev_hash):HB/bitstring,
  (H#header.height):HtB,
  (H#header.time):TB,
  (H#header.version):VB,
  (H#header.trees_hash):HB/bitstring,
  (H#header.txs_proof_hash):HB/bistring,
  (H#header.difficulty):DB,
  (H#header.nonce):HB
>>.
```



## TODO

Check in codebase and describe computation of difficulty. E.g. every 2000th block retargets the difficulty based on the previous 2000 block headers.

Confirm from the codebase that `#leaf.meta` (as opposed to `#leaf.value`) is not meant to be exchanged on the network as part of the data structures described in this document, then reconsider using the term "value" rather than "datum" for leaves of Merkle trees.

Detail Merkle tree and Merkle proof.

Decide whether to include `#header.accumulative_difficulty` in this description.

Confirm exclusion of `#block.prev_hashes` from this description.



a governance variable limits the total size of the transactions in a single block.
the other block parts are also limited. Height must be an integer one greater than the previous.
prev_hash must be the output of a hash function, which is fixed sized.
channels is the root of a trie, which is the output of a hash function.
accounts is the root of a trie.
mines_block must point to an account id, which is limited, or a tuple of an account id and an address, which is limited in the account:serialize function.
 time must be less than the current time. and greater than 0.
 difficulty must be calculated from the previous difficulty.
 the comment must be less than 140 bytes.
 the magic number is fixed.

so, the block is limited in size

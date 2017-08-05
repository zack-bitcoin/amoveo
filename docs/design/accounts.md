# Accounts

The following describes accounts, which are one of merkel trees, responsible for keeping address and balance associated with it.

## Overview

Account consists of:
* Pubkey
* Balance - amount of money that is associated with an account
* Nonce - number incremented with every transaction that is put on the chain
* Height - height of account last update
* Bets - pointer to merkle tree that stores how many bets have been made in each oracle
* Shares - pointer to merkle tree that stores how many shares at what price account has

## Implementation details

#### Account representation

Account structure is represented by Erlang record:

```erlang
-record(acc, {pubkey = <<>>,
              balance = 0,
              nonce = 0,
              height = 0,
              bets = 0,
              shares = 0}).
```

#### Merkle trie form

Accounts are stored in `tree` in the following way:
* Key = integer representation of Pubkey
* Value = serialized account record (described below)
* Meta = concatenation of account's `bets` and `shares`

Serialized account is Erlang binary consisting of:
* balance
* nonce
* height
* pubkey
* hash of pointer to bets merkle trie
* hash of pointer to shares merkle trie

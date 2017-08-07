# Accounts

The following describes accounts, which are one of merkel trees, responsible for keeping address and balance associated with it.

In order to create an account one has to pay fee, which prevents spam.

Account deletion is rewarded, which incentivizes the reclaiming of space.

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

#### Creation and deletion

During account creation one has to pay `create_account_fee`, driven by governance.
Account creation transaction is defined in `create_account_tx.erl`.

During account deletion `delete_account_reward` is paid.
Account deletion transaction is defined in `delete_account_tx.erl`.


#### TODO

Describe `create_acc_tx` and `delete_acc_tx` records.

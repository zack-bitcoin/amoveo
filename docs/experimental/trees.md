How many merkel trees are there? What do they store?

* proof of burn
* proof of existence
* accounts
* channels
* active oracles
* oracle results
* shares
* oracle bets

=== proof of burn

The proof of burn tree stores by address. It stores the number of AE tokens that this address has burned.

=== proof of existence

This tree stores by hash. It stores the same hash that you use to look up the hash.

=== accounts

This tree stores accounts by integer id. Each account has 2 merkel roots. One is for a shares tree, the other is for an oracle bets tree.

=== channels

This tree stores channels by an integer channel id.

=== active oracles

These are the oracles that exist right now. They are stored by integer oracle id. Oracles never reuse the same id.

=== oracle results

These are the results of oracles that have existed. They are stored by id.

=== shares

Each account has a tree of shares. The shares are stored by share id. Each type of share has a different id. The id of a share determines it's difficulty. You can own either a negative or positive amount of each type of share. Shares are transferable

=== oracle bets

Each account has a tree of oracle bets. Oracle bets are not transferable. Once an oracle is settled, the bets in it can be converted to shares.
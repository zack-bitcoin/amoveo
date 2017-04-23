There are 8 merkel trees.

* oracles+
* orders
* channels+
* accounts+
* oracle bets
* shares
* proof of burn+
* proof of existence+

(The ones with +'s are included in the generation of the state-hash that is recorded on the block's header)

=== oracles

These are the oracles that exist right now. They are stored by integer oracle id. Oracles never reuse the same id.
The hash of the text of the question is stored.
These are the results of oracles that have existed. They are stored by id.
This data is available to the VM.
The result is stored in 1 byte. Either it is 0 for false, 1 for true, or 2 if the questions was bad, or a 4 if the question hasn't been answered yet.

=== Orders

Every oracle has an order book. The order book is a linked list of orders. Each order has an amount, and the id of the owner.

=== channels

This tree stores channels by an integer channel id.

=== accounts

This tree stores accounts by integer id. Each account has 2 merkel roots written in them. One is for a shares tree, the other is for an oracle bets tree.

=== oracle bets

Each account has a tree of oracle bets. Oracle bets are not transferable. Once an oracle is settled, the bets in it can be converted to shares.

=== shares

Each account has a tree of shares. The shares are stored by share id. The id of a share determines it's difficulty. You can own either a negative, positive, or zero amount of each type of share. Shares are transferable
[you can read about shares here](docs/shares.md)

=== proof of burn

The proof of burn tree stores by address. It stores the number of AE tokens that this address has burned.
This data is available to the VM.

=== proof of existence

This tree stores by hash. It returns a 1 if the thing exists, a 0 otherwise.
This data is available to the VM.

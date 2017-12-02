There are 8 [merkel trees](/docs/basics/merkle.md).

* oracles+
* orders
* channels+
* accounts+
* oracle bets
* proof of burn+
* proof of existence+
* governance+

(The ones with +'s are included in the generation of the state-hash that is recorded on the block's header)

=== oracles

These are the oracles that exist right now. They are stored by integer oracle id. Oracles never reuse the same id.
The hash of the text of the question is stored.
These are the results of oracles that have existed. They are stored by id.
This data is available to the VM.
The result is stored in 1 byte. Either it is 0 for false, 1 for true, or 2 if the questions was bad, or a 4 if the question hasn't been answered yet.
[read more about oracles here](oracle.md)

=== Orders

Every oracle has an order book. The order book is a linked list of orders. Each order has an amount, and the id of the owner.

=== channels

This tree stores channels by an integer channel id.
[more about channels](channels.md)

=== accounts

This tree stores accounts by integer id. Each account has a merkel root written in it. It is for the oracle bets tree.
[more about accounts](accounts.md)

=== proof of burn

The proof of burn tree stores by address. It stores the number of AE tokens that this address has burned.
This data is available to the VM.

=== proof of existence

This tree stores by hash. It returns a 1 if the thing exists, a 0 otherwise.
This data is available to the VM.

=== governance

This tree stores by atom name. It contains many variables that define the consensus protocol. The oracles can be used to update these variables.
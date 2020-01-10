Sortition Chains
=========

This is a scaling plan, similar to sharding or side-chains.

Related documents:

[sortition chain attacks analyzed](./sortition_chains_defense.md)

[sortition chain implementation details.](./sortition_chains_implementation.md)

[sortition chain theory.](./sortition_chains_theory.md)

[sortition chain random number generator.](./sortition_chains_random.md)

[sortition rollup](./sortition_chain_rollup.md)


What is a Sortition Chain?
=========

Sortition chains are a lottery-type sidechain.

A sortition chain is a kind of lottery.
The lottery tickets are spendable and divisible.
The database of lottery tickets is off-chain.
Eventually we draw the random number to determine who won the lottery.
The winner publishes the slice of the lottery ticket database that proves that they won.
No matter how many people are owning lottery tickets, the proof of who won can stay small.

Sortition chains are a tool that combine features of probabilistic payments with state channels. 

If you own $10 in a sortitoin chain that has $1000 total locked in it, that means you have a 1% chance to win $1000, and a 99% chance to win $0.

A person usually doesn't want to hold a contract that only has a 2% chance of having value. That is a lot of risk. But as long as there are other people willing to buy the contract at a good price, this works.

You can create sortition chains inside of existing sortition chains, allowing for exponential scalability.

Sortition Chain Operators
======

Each sortition chain has a team of operators. Updating the state of the sortition chain requires all N of N of the operators to sign the new state root.
Requiring all N of N to sign means we can be sure at least one of them will keep the data available.

They use Tendermint consensus to agree on what commitment should be signed on next.
That way we can know that any double-signing is not accidental, it was malicious.
Not that we can punish operators. We just want to identify which ones are malicious so we wont hire them as operators again in the future.

The commiters commit the merkel roots of 2 trees. The first tree divides up the probabilistic value space between the different people who own it. The location in the tree is determined by which RNG values would result in your winning. That way, a merkel proof of your part of the tree is also a proof that no one else is owning that part of the probabilistic value space.

The second tree contains a bunch of signed statements where people give up ownership of parts of the probabilistic value space, and where people declare that they want the part of the value space that they own converted into a new sub-sortition chain that is inside of the existing parent sortition chain.


Embedding smart contracts
========

When people sign a message that they are giving up control of part of the sortition value space, they can include a commitment to a smart contract. That way they are only giving up control if someone can provide evidence to make the smart contract return "true". Similar to unlocking a bitcoin UTXO.

This allows us to use turing complete contracts to determine who owns which parts of the probabilistic value space.

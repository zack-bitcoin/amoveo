Sortition Chains Implementation
============

[sortition chains home](./sortition_chains.md)

tx types
====

1) sortition new

* pubkey who initially owns all the value.
* amount of money for the lottery prize.
* expiration date for when it it becomes possible to make sortition-contract-txs for this sortition chain.

2) sortition split

This splits a sortition chain into two smaller sortition chains.

C = a binary chalang contract to determine how the old sortition chain is split. 

for sortition chain 1, it selects a random value such that C is true.
for sortition chain 2, it selects a random value such that C is false.

3) sortition claim

* show what height at which you had a claim to the winning part of the probabilistic value.
* You pay a safety deposit.
* this potential winner gets added to a list of potential winners.

4) sortition evidence

* which potential winner did not win.
* evidence to prove that they had signed away ownership of the winning part of the probability space.
* they get removed from the list of potential winners.

5) sortition timeout

* whichever potential winner has the highest priority claim to win, they win the lottery.
* a claim linked to an earlier block height has higher priority.
* this tx can only be made if you wait a sufficient amount of time after the RNG to choose the winner was generated.

6) proof of existence

* This allows the creator to publish 32 bytes of data into the proof of existence tree. It keeps a record of the block height at which this hash was recorded.
* This is used for hashlocking, because we need to prove at what height a pre-image was available.
* This is used for sortition operators to record the fact that they have signed a merkel root of a sortition database.


New Merkel Tree Data Structures in the Consensus State
============

1) probabilistic value space

id is a subset range of values in [0,1]

* pubkey of who owns this lottery ticket, or a pair of pubkeys if it is a channel, or a list of operators if it is a baby sortition chain.

2) waivers

id is generated from the sortition ID and pubkey of who is giving up control.

* signature
* sortition chain ID
* pubkey of who is giving up control

3) proof of existence

id is the 32 bytes being stored.

* arbitrary 32-bytes.
* the height where this was recorded.

4) potential_winners

for every potential winner there can be multiple layers of sortition chains in their proof that they won.
for every layer of sortition chain, we need to remember a priority height as well as a list of pubkeys of accounts assigned to collect evidence for when users give up ownership of parts of that layer.

the priority height is the block height when this data was commited into the blockchain. Earlier heights are higher priority.

consensus state consumed O((number of potential winners)*(# layers of sortition chains deep)*(# accounts assigned to gather evidence in each layer))

each pw_root contains a pubkey of who could win.

so, every pw_root will point to the next pw_root in the linked list. it is ordered based on priority.
and the pw_root also points to a pw_layer.

5)  potential_winner_layer

each pw_layer contains a priority height for that layer.

each pw_layer has a pointer to the pw_root it is associated with.

the pw_layers are a linked list, each pointing to the next. they are ordered based on the order of the sortitoin chains inside of each other.


6) potential_winner_spent_proofs

each pw_spent_proof contains the pubkey of the account assigned to collect spent proofs into a merkel tree.

each pw_spent_proof_operator has a pointer to the pw_root it is associated with, and to the pw_layer it is associated with.

the pw_spent_proofs are a linked list, each pointing to the next. 


Timeline for horizontal payment
=============

Bob has veo in a sortition chain, he wants to spend to Alice, and he wants the option to hashlock this payment against something else.


1) Alice downloads all the history of all the merkel proofs of Bob's part of the probability space. She downloads this history from any of the operators, or from Bob.

2) Bob gives the operators a signed message explaining how he wants to pay part of his money to Alice. He makes a signed message saying that if a particular pre-image is revealed, he will give up ownership of his part of the probability space. 

3) Alice verifies that the signed message about Bob giving up ownership is committed. She verifies that there was a commitment giving her 2nd highest priority ownership of Bob's part of the probabilistic value space. Now Alice can know if the pre-image is revealed, that she will own the value.

4) Alice sets up the other half of the hashlock using the same commitment. To pay Bob.

5) Bob reveals secret S to Alice. So now Alice controls the value Bob had wanted to spend to her.

Timeline for vertical payment
===========

Bob has veo in a sortition chain, he wants to spend to Alice, and he wants the option to hashlock this payment against something else.

1) Bob gives a signed message to the operators explaining that he wants to convert his account into a baby sortition chain. He gives the new list of operators for the new sortition chain. This message gets committed on-chain by the operators.

2) Alice downloads all the merkel proofs for the sortition chain, so now she knows about the baby sortition chain.

Bob owns 100% of the value in the new sortition chain, so he can do horizontal payments like normal.







<!-----



Data the sortition chain operator needs to store
=============

1) We need to store all the merkel proofs so that we can prove to users that parts of our probability space are unowned.

```
(number of elements in your sortition chain) * (number of blocks that the sortition chain exists for) * (number of ancestors between you and the main chain) * (size of a merkel proof in bytes)
1000 * 2k * 6 * (32 bytes * log2(1000))
```
is like 0.64 gigabyte of data per sortition chain



for each user, merkel proof of that sortition contract such that it is stored based on the part of the probability space that results in this sortition contract winning the lottery.
```32 * log16(#of live sortition contracts)*(# of live sortition contracts) bytes```
So if there are 1000 users, and about 1 trade happens per second, and the sortition chain lasts 2 months, then this will take up 32*log16(1000)*5000bytes = about 400 kilabytes per user, or 400 megabytes for everything.

2) for every sortition contract that has existed in his sortition chain, he needs to store a signed message where the user has agreed that the old version of the sortition contract is invalid. The signed message has a commit-reveal. He also needs to have the secret which was revealed for this commit-reveal.

signature + contract hash + commit + reveal
```(about 250 bytes) * (# of sortition contracts)```
If there are 10k users over the cource of the 2 month period, we are looking at 2.5 megabytes of data.

3) a copy of every live smart contract with every customer, the customer's signature over the contract.
Generally a market's contracts are repetitive, so each can be comopressed to 100 bytes or so. The signature is like 150 bytes.
So if there are 1000 live sortition contracts, this database takes up around 250 kilobytes.


Collapsing spend proofs
=============

For example, lets say that you are running a sortition chain, and one of your customers controls 10% of the value in your sortition chain.
This customer makes thousands of tiny payments per day, spending and receiving fractions of a percentage of the sortition chain.

We don't want to have to store a seperate spend-tx every time this one customer spends more of the value they own in the sortition chain.

So lets say they own [0.15, 0.2].
At first the spend tx could give up ownership from [0.15, 0.155], and then we could make a new spend tx to give up ownership from [0.15, 0.16].
The new tx includes the same range of coverage as the old, so we do not have to store the old one any more.

This is why we only need to store one spend tx per pubkey being used in our sortition chain.

Data the Users Need to Store for a small wallet
==============

1) for your sortition contract, you need to keep a copy of the most recent contract state signed by the sortition chain operator. So 150 bytes of signature, plus however long your smart contract is.

2) you need the keep a merkel proof showing when your sortition contract was first created. You can use this proof to punish the sortition chain operator if they try to double-spend your money.
256*(log16(number of sortition contracts in your sortition chain)) = about 1280 bytes.

3) you need to download a merkel proof of the part of the probability space that you want to buy. You need this proof for every merkel root that the server recorded on the main chain from the start until when your contract was first created.
You don't need to store this, you just verify it once.
512*(log2(#users)) * (number of blocks in the sortition chain) = about 3 megabytes.

4) you need to keep a copy of the txs of anyone else who had previously owned part of the probability space that you now own, so if they try to claim it, you can stop them by showing that they gave up ownership.
For a small account, this is probably about 10 signed txs with short contracts. Around 20 kilobytes.


Lightning Sortition Contract Creation
===========

a 2.2 secure way to do a lightning payment to create a new sortition contract.

But it can only work if
* you have an existing sortition contract capable of accepting as much value as you will control in the new sortition contraxt
* there is a lightning path between you and the person that you want to make the new sortition contract with.

The trick is that if you want a new sortition contract, then you just buy insurance against the possibility that it will not get created.
You use an existing lightning path to buy this insurance.

Once the sortition contract is created, then the insurance contract can never pay out, so it can be deleted.

The cost of buying insurance is far less than the volume of money that can be held in your sortition contract, because you can buy an empty contract.

The cost to the server is (6 block periods of confirmations) * (interest rate) * (amount of money which you will be able to receive I the new sortition contract)

So this is a way to instantly increase the amount of value you can potentially receive in sortition contracts more than 1000x, within a few confirmations, trustlessly.

But, you need an existing sortition contract before you can do this trick.

The value that you control in the sortition contract that is about to be created needs to be less than your total insurance coverage.
Your total insurance coverage needs to be less than the amount of veo you could possibly receive in existing sortition contracts.

Mixing Merkel proofs with contracts
=========

We want the merkel proof to be a proof that the server has not double-spent your part of the probability space. So we need bits of software in every branch of the merkel tree.

A merkel proof looks like this.

```proof = [stem, stem, stem, leaf];
leaf = {address, contractID};
address - this is the address of the person who owns the sortition contract being proved.
stem = {chalang_bool, evidence, hash1, hash2} // if chalang_vm(evidence ++ chalang_bool) returns true, then hash1 needs to match the hash of the next element of the proof. if it returns false, then hash2 needs to match the next element of the proof.
// chalang_bool - this is some chalang code. It outputs 2 values. the first is a true/false for which branch of the merkel tree is being executed. 0 is false, any other value is true. The second value is the nonce for this version of the contract.
```

We need to keep track of gas while verifying merkel proofs, since the chalang_bool step is turing complete.

example chalang_bool contracts:

`int 7 int 13 rand_bit` This contract has a 7/13th chance of returning `true` and a 6/13th chance to return `false`

`OracleID oracle_lookup dup int 2 == if int 3 int 4 rand_bit else drop drop then` This contract returns `true` if the oracle is `true`. it returns `false` if the oracle is `false`, and if the oracle is `bad_question`, it has a 3/4ths chance of being `true` and a 1/4th chance of being `false`.



So lets make a very concrete example of a merkel proof.
Lets imagine Bob bought a contract worth $20, and the sortition chain has $1000 in it.

rand_int is a new function we need to add to chalang. it takes 3 integers as input, and uses the random seed embedded into the merkel proof verifier for entropy.

rand_int ( A B -- true/false )

It is required that A =< B.

It has an A / B probability of returning true, and (B-A) / B probability of returning False.


[{stem, [], compile("int 1 int 100 rand_int"), hash1, hash2},{stem, [], compile("int 1 int 2 rand_int"), hash3, hash4}, {stem, [], compile(" OracleID0 oracle_lookup dup int 2 == if int 1 int 2 rand_bool else drop drop then "), hash5, hash6}, {address, contractID]

where

```
hash1 = hash(compile("int 1 int 2 rand_int") ++ hash3 ++ hash4;
hash4 = hash(compile(" OracleID0 oracle_lookup dup int 2 == if int 1 int 2 rand_bool else drop drop then ") ++ hash5 ++ hash6;
hash5 = hash({address, contractID});
```

and given the random entropy programmed into this sortition chain, the "int 1 int 100 ran_int" contract needs to return true, and the oracle needs to either return true, or it's bad question version needs to randomly select true. and the "int 1 int 2 rand_int" contract needs to return false.

So this is a contract that has a 1/200th the value of the sortition chain it is a part of, and it is a winning bet in an oracle.


entropy for contracts
=============

If our merkel tree has multiple tiny contracts, it would be inefficient to re-load the block hash into the random number generator over and over for every contract. and it would be bad if we re-used the same random bits on multliple contracts, because that could cause correlation between the multiple layers, so some lottery tickets would be worth double what we had intended, and other lottery tickets will be worth nothing.

example tree update
============

`operator` is who made the merkel tree

tree starts empty, which means 100% chance it goes to the operator. `operator`

Bob buys 1% of the value`{"int 1 int 100 int 1 rand_bool int 1", bob, operator}`

Alice buys 2% of the value `{"int 1 int 100 int 1 rand_bool int 1", bob, {"int 2 int 100 int 1 rand_bool int 1", alice, operator}}`

Carol buys 1% of the value `{"int 1 int 50 int 1 rand_bool int 1", {"int 2 int 100 int 1 rand_bool int 1", alice, bob}, {"int 3 int 100 int 1 rand_bool int 1", carol, operator}}`

The key to look up your name in the merkel tree is any entropy value and oracle values such that you would win.
By using that as the key, the sortition chain operator is free to rewrite the contracts as necessary to keep everything in balance.

the rules for updating the tree need to be deterministically defined somewhere, that way anyone syncing the sortition-blocks will calculate the same root.


Contract Table
=========

Many contracts are appearing repeatedly. So we should keep a single copy of them in a different table, and refer to them by hash.
For example, if 4 people are betting on 4 mutually exclusive outcomes, the tree could look something like this:
```
{Contract1, {Contract2, Alice, Bob}, {Contract2, Carol, Dan}}
```
If contract1 and contract2 are both long, then it is best to avoid having to write one of them out twice in the database.



---->
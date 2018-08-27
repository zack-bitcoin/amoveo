## How to program smart contracts inside of channels.

Table of contents

* structure of a smart contract
* merkelized abstract syntax trees
* hashlocking
* contractlocking
* how much veo should be locked in the contract
* output of a smart contract
* tips

## structure of a smart contract
A smart contract is broken up into 2 parts, a script sig and a script pubkey.
They are named after 2 parts of the program that makes a bitcoin tx.
The script pubkey is a channel contract that both participants in the channel signed.
The script sig is extra evidence that you provide to defend your case as to why the veo in the channel should be distributed a certain way.
The script pubkey is signed by both participants, the script sig doesn't require signatures.
Internally, Amoveo is processing smart contracts using the chalang VM, a virtual machine optimized for smart contracts in channels.
Amoveo first feeds the script sig into the VM as a valid chalang program. the state of the VM is maintained, and the script pubkey is next fed into the VM as a valid chalang program.
Any functions defined in the script sig are still available when executing the scriptpubkey.


## merkelized abstract syntax trees
This basically means to put a smart contract into a merkel tree, so that you can verify merkel proofs of the code that actually gets executed, and you don't have to learn about any code that does not get executed.
So a smart contract could have terabytes of data encoded into it, but when you publish it on the blockchain, it only costs 32 bytes.
In chalang this is done using functions. The name of a function in chalang is the hash of it's contents. So you can call functions in the script pubkey, and if you wanted to use one of them, you would provide the definition in the script sig.

## hashlocking
Hashlocking is the key technique that allows for atomic swaps, and for lightning channel payments.
With hashlocking you make 2 different transactions that are each not valid until a secret is revealed.
Once the secret is revealed, they both simultaniously become valid transactions.
There is no way to make only one of the two valid, it is either both or none.

## contract-locking
contract locking is a generalization of hashlocking.
"If the same secret-revealing program is written into 2 payments, then they are locked together." generalizes to -> "if the same program is written into 2 payments, then they are locked together."
You can also think of this as arbitrage. If you buy a contract and simultaniously sell the same contract somewhere else, then your risk cancels out. you don't gain or lose.
You can also think of this in terms of chess. If I could simultaniously play the 2 best chess player, I would either win one of the games, or tie both.
The trick is to copy the moves from each game to the other, so the grandmasters are playing each other through me.

We use contract-locking to connect smart contracts from different channels together, to allow for dapps with more than 2 participants.

## how much veo should be locked in the contract
Alice and Bob have a channel, they are making a smart contract.
If Alice can win up to A veo, and Bob can win up to B veo,
then;  Bob will need to put A veo into his side of the channel, and Alice will need to put B veo into her side of the channel.

Over time as the contract is updated, the total amount of veo each participants needs to leaved locked in the contract will decrease. This extra Veo can be removed from the smart contract and reused for other smart contracts.

## output of a smart contract
chalang smart contracts output 3 numbers.
* the amount of veo being transfered from 0-10000
* a nonce, for how much priority this outcome has.
* a time-limit, for how much time the other channel participant has to provide evidence with a higher nonce.


## Tips:
* smart contracts often need to be invertible, so the customer can participate on either side. In order for the channel hub to arbitrage, each customer needs to be making exactly opposite bets.
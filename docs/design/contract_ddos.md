The attack works like this:
The attack writes a big contract that is expensive to verify. We don't know that it is invalid until we finish verifying it.

Ethereum solution:
The transaction is still valid, the attacker pays enough gas that the attack is too expensive to commit.

Amoveo solution:
If you want your transaction included in a block, you first pay the channel server to help you build the transaction. The channel server is a shard that keeps track of a fraction of the blockchain's consensus state. The channel server builds the transaction and makes a proof and gives it to several miners. The channel server's service is dependent on being able to get transactions published quickly.
So the channel server doesn't do anything weird when interacting with miners.
If the miners lose money from their relationship with a particular channel server, the miner can choose to stop accepting transactions from that shard.


Benefits to Amoveo's choice:
The Ethereum VM has to be carefully constructed so it wont crash from any kind of input. Proving that it wont crash is impossible, so it is impossible to prove that it is secure.
The Amoveo VM is allowed to crash. When the Amoveo VM crashes, it doesn't impact the security at all.
Since crashing isn't a security vulnerability, it is possible to prove that Amoveo's VM is secure.

Benefits to Ethereum's choice:
You don't need payment channels
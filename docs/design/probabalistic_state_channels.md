Probabilistic State Channels
==========


scalability
======

Originally, the bitcoin scaled by the number of payments. The cost to the bitcoin system grew more or less linearly with the number of payments.

Channels meant that we could scale with the number of financial relationships.

channels + probabilistic payments meant that we could scale with the number of users.

probabilistic channels means we can scale with the number of channel hubs connecting the network together.

So for the same cost as making 1 payment per week on Bitcoin, you could create and maintain a channel hub on Amoveo and have tens of thousands of channel relationships, all with different turing complete smart contracts being updated every second.
It is a hugely more scalable design.


liquidity in off-chain smart contracts
=========

A major limitation of channels is that they are terrible for lottery.
You can only win as much money as is in the channel.
prob-channels don't have this problem.
If you lock more than $1000 in stake, you could make 1000 channels, each with a 0.1% chance of winning $1000.

Similarly, if you are running a market powered by probabilistic state channel smart contracts, you could sell many mutually exclusive channel contracts using the same staked funds.
The only channels which can ever get created on-chain are the ones where all the other channels cannot get created.



tx types
====

1) probabilistic-deposit

* amount of money
* pubkey to control spending

2) probabilistic-withdraw

* a chalang spk signed by the owner which, if unlocked, enables this withdraw.
* a chalang script-sig to provide evidence to unlock the spk.
* if chalang_vm(script-sig ++ scipt-pubkey) returns true, then it is valid.


3) probabilistic-cancel

If a prob-channel has already been closed, and someone tried doing a probabilistic-withdraw for it anyway, this is how the channel hub provides evidence to prevent those bad probabilistic-withdraws.

4) probabilistic-channel

* you have to wait a long enough delay after the probabilistic-withdraw before you can do this tx.
* if there is more than one active valid probabilistic-withdraw for the same probabilistic-account, then delete 90% of the money, and give the rest to whoever published the 2nd probabilistic-withdraw, otherwise continue.
* this creates a normal on-chain channel, just like a new-channel-tx would. Once created, the spk that controls the funds in this channel is not necessarily at all related to the spk from the probabilistic-withdraw step.
* the channel has 80% of the money from the prob-account, the last 20% goes back to whoever made the prob-deposit initially.

5) proof of existence

* This allows the creator to publish 32 bytes of data into the proof of existence tree. It keeps a record of the block height at which this hash was recorded.



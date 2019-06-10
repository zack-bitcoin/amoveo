Probabilistic payments
===========

a probabilistic payment (PP) is a kind of transaction that has a small probability of transfering funds. It only has to be written to the blockchain if the funds actually get transfered.

This is a way to reduce the amount of information that gets written to the blockchain. Instead of receiving 1000 payments per day each worth 1 coin, you can receive 10 payments each worth 100. Which means only 10 tx fees need be paid, instead of 1000.

PPs excel in exactly the kinds of situations where channels fail.
Lottery type contracts are the hardest to program in channels, because every person who can win money needs to have their own pool of liquidity that they can win from.
With PPs, everything has to be formatted in terms of lotterys, and there is almost no money locked up.

Probabilistic Payment Transaction Types
==========

* moves some money into a new probabilistic-payment-account.
* makes a probabilistic payment from your pp-account.
* wait a delay after (2) before you can make this tx. This unlocks the money from the probabilistic payment. You can only make this tx if there has been exactly 1 (2) for this pp-account. 1/5th of the veo goes back to the person who created the pp-account, and 4/5ths goes to the person receiving the payment.

if multiple (2) get published for the same pp-account, then 9/10ths of the money from that account is deleted, and 1/10th goes to the miners, and the tx fees for all the (2)'s after the first get deleted.
The miner fee for (2) needs to be less than 1/10th of how much money is in the pp-account.

PP account state
=======

* pubkey for spending from this account.
* quantity of veo locked in this account.
* how long the wait period is for submitting counter-evidence to prevent double-spends


Kinds of Probabilistic payments
=========

There are 2 ways of using PPs.
1) If the amount of money in your PP-account is low, or the amount you are sending is large, then the recipient will need to wait a some block confirmations to verify that the tx was valid.
2) Otherwise, the recipient can have immediate confirmation that the money was sent.

specifically
```
N = number of people who can accept PPs of this amount of veo for 0-confirmation payments
P = portion of money from PP-account being sent in this PP
N*P<1
```
If N*P<1, then you don't have to wait any confirmations, and you can maintain 2.2 level security.

instead of N*P<1, it would be more accurate to say: "The amount of veo in your PP-account is greater than the sum of the value of things available for sale during a 2-block period in exchange for PP, where they allow 0th confirmation payments."


The Lightning Network
==========

If N*P<1, then you can use this PP as one step in the path of a lightning payment.
This enables an alternative form of the lightning network, more powerful than the hub-and-spoke model.

If we use probabilistic payments, then every lightning payment has exactly 3 steps.

1) consumer to their hub as a channel payment.
2) hub to hub as probabilistic payment.
3) and then hub to vendor as a channel payment.

So there is a lower amount of liquidity that needs to be locked in channels for everyone to be connected, it only grows linearly with the number of users.

Any 2 hubs can connect with probabilistic payments. so it is a big salability improvement.

Lets compare this to the lightning network that lacks PP:
If each channel hub has N channels with users, and is connected to M other channel hubs, and P is the number of customers, then I think the number of channels with a pure-channel lightning network is P*(1+(M/(2*N)))
And the number of hops for a lightning payment is logM(P/N)
The number of fees paid per lightning payment is logM(P/N) - 1.

With the combo, the number of channels is just P, there are 3 hops per payment, and only 2 fees paid per payment.

With pure probabilistic payments, the number of channels is 0, there is 1 hop per payment, and ~0 fees per payment, but each user is taking huge risk in an unacceptable way.

With pure lightning, every channel operator is always watching the balances of their channels to try and stay useful so they can get fees. It is computationally expensive.
They have to charge fees to cover the computation cost.
Sometimes there is a scarcity of veo available for certain lightning paths. This scarcity can drive up the cost of making payments.

With the combo, none of the operators every have to care about channel balances.
They can instantly make payments to any other operator.
There is never a scarcity of veo in certain places causing higher fees.
Each customer has to think about their channel balance, and that's it.


Programmable Probabilistic Payments
========

The ability to do hash-timelock PP hints at an even more powerful version of PP.
We can embed a turing complete smart contract into the PP to determine the probability that the payment should happen.
It can't be a very long-lived contract, because the requirement that N*P<1 can only protect us if we can execute the tx within the delay coded into the PP-account making the payment.

This allows for more complicated contracts to unlock the PP besides hashlocking. Examples:
* Bob could incentivize Alice to ask the Amoveo oracle a new question by creating a PP that can only pay Alice if she makes the oracle.
* more generally: you could have a PP that can only execute if Alice publishes a signature she generated with her private key over some data that Bob wanted her to sign.
* You could make a PP where the amount of money being sent is determined by the price of the next batch in an off-chain Amoveo market, so you could make a payment worth $5, regardless of the $ vs VEO exchange rate.
* you could make a bunch of PP so that instead of on average only one is valid, it could be set up so precisely one will be valid. Which could be useful for lottery games.
* gambling games like satoshi dice could be set up more optimally. When you pay to play, you should be using channels. But when you get paid for winning, it should come from a PP. This way you have the advantage of channels; you can play many times without paying any miner fees. and you have the advantages of on-chain txs; you don't have to pay the server to lock up the money that you can win.


Probabilistic Payments that Anyone can Accept
==========

Sometimes you want to pay for something to get done, and you don't care who does it. For example, you might want to make a contribution to have a new oracle created, or you might want to pay a miner to include a tx without recording the tx fee in the tx.

The tx types for this kind of probabilistic payment:
1) the tx that initializes a payment from your pp-account, includes a commitment to some entropy.
2) begin the process of unlocking the payment by providing evidence that you have created the oracle.
3) the person who initialized the payment reveals the entropy they committed to.
4) unlocks the payment depending on the random entropy from state (3), this tx might be valid.


* this probably means that the limit order tool for making new channels is not secure. An attacker can make out of network payments to the miners to get their tx included.


Random sampling without replacement
=================

We can cancel out risk in the quantity of veo being transfered by probabilistic payments by reusing entropy and doing random sampling without replacement so that the different probabilistic payments are mutually exclusive.

Thinking of a more concrete example.
There are 10 channel hubs, and each one makes a probabilistic account with 10 bitcoin of value locked in it.
All 10 hubs choose the same block height for entropy in the future, that way the probabilistic payments are all correlated.
So now the hubs are routing lots of payments between each other.
Occasionally one of the hubs will run out, having make 10 bitcoin worth of payments. In that case they need to open a new probabilistic account before they can route more payments.

Eventually, there is only a few hours left until the entropy will be generated to determine which of the probabilistic payments will get recorded on-chain.
At this point, all the hubs have probabilistic accounts that had 10 bitcoin in total, and are partially emptied.
If the hubs have channels on each other's servers, they can send any remaining bitcoin from their probabilistic accounts into channels on each other's hubs.

Since the hubs have always been routing payments through each other, it is always possible for them to completely cancel the risk in everyone's probabilistic accounts by draining all the accounts completely, and carefully controlling for which range of the random sampling they are using when receiving payments.

If I received a 1 BTC payment correlated with range [0, 0.1], then I want to make sure any more payments I receive are in [0.1, 1].

It is important that all the probabilistic accounts where we are canceling risk this way, they all need to have the same total balance. That is why in my example they all have 10 bitcoin in their probabilistic accounts.

If I have received 9 bitcoin from probabilistic accounts that each contain 10 bitcoin, and I have received on the intervals [0, 0.5] [0.6, 1], that means I have risk if the randomness chooses in the interval from [0.5, 0.6]

So that means I need to pay to receive a 10% chance of 10 bitcoin as long as the entropy ends up in [0.5, 0.6]

One way to achieve this is if I receive 2 payments, each giving me a 10% chance of receiving 5 bitcoin, and both payments are only valid if the entropy ends up in the interval [0.5, 0.6]


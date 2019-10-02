## Amoveo White Paper

## Table of Contents
* Abstract
* Motivation
* Blockchain Consensus Protocol
* Accounts
* Oracles
* Governance
* Channels
* Smart Contracts Inside Channels
* Derivatives, not Subcurrency
* Chalang Smart Contract VM
* Lightning Network
* Markets
* Light Nodes and Sharding
* Example Use Cases
* Oracle Game Theory


## Abstract

Blockchain is a form of cryptographic database technology. Each blockchain hosts a cryptocurrency. Cryptocurrencies are a form of wealth that is made secure by blockchain technology.
Amoveo is a blockchain which secures a cryptocurrency called Veo.
Using Amoveo you can host a market on a server where you make money every time anyone trades.
Using Veo in the markets, you can buy and sell risk in anything.
With Amoveo it is impossible for customers to steal from a market, and it is impossible for the market to steal from its customers.


## Motivation
Separating church and state is good. It makes both the church and the state less dangerous.
Combining religious convictions with a monopoly on violence was deadly. It meant that people could be easily convinced to kill you at any time, and there was no consequence to the religious leader who called for your death. 

Separating finance and state will also be good. It will make both the state and finance less dangerous.
Combining finance with a monopoly on violence means that we can't truly trust the finance system. The people who are holding our money are able to steal our money without consequence. This is a terrible combination.
[Amoveo mission statement](mission_statement.md)


## Blockchain Consensus Protocol
Amoveo uses Nakamoto consensus, just like Bitcoin. In this system, some people act as miners. They receive a reward for producing blocks. Producing a block involves doing an expensive calculation called proof-of-work. The difficulty of this proof-of-work changes so that the rate of block production stays about 10 minutes.


## Accounts
Accounts are data structures recorded in the blockchain consensus state. Each account has a positive balance of Veo that it can spend to other accounts. Anyone with sufficient Veo to pay the fee, can create new accounts. Spending from an account requires a signature from the private key that the owner of the account knows. To give someone Veo, you need to know their public key.

You use accounts by making transactions. [Read more about the Amoveo transaction types here](design/transaction_types.md).
Accounts are stored in one of the consensus state merkle trees. [Read more about the trees used in Amoveo here](design/trees.md).


## Oracles
Anyone can ask the oracle any question if they are willing to pay the cost.
The oracle can settle in 3 states: True, False, Bad Question.
Unlike some competitors, Amoveo does not have a subcurrency to power the oracle. Because this subcurrency doesn't exist, the users of the oracle don't have to pay rent to the owners. This means Amoveo's oracle will be more affordable than competitors whose oracles depend on subcurrencies such as Augur or Bitcoin Hivemind.
The Amoveo oracle does not usually have much collateral at stake, so the cost of launching an oracle is small. To launch an oracle, you pay just enough to provide initial liquidity to get a market going.
Amoveo's oracle has the ability to escalate its defense in response to an attack. Oracles that cannot escalate are prohibitively expensive, or they aren't secure.
It is this ability to escalate that allows Amoveo to keep the collateral for each oracle so low most of the time.
[Read more about oracles here](design/oracle.md)


## Governance
Using the oracle, the parameters that define the system can be modified. The mechanism is built with the goal that the oracle will choose parameters that make the value of Veo increase.
[Read more about governance here](design/governance.md)


## Channels
Publishing a transaction to a blockchain is time consuming. You need to wait for the transaction to be included in a block, and then depending on how much money is moved, you need to wait for more confirmations. Waiting for confirmations means that you wait for enough blocks to be added to the blockchain for enough security. Trying to avoid this time-consuming process as much as possible is why we use channels.

Channels are two party relationships recorded on the blockchain. Each channel has a finite amount of money it controls, just like an account.
Once two people have a channel together, they can instantly move the money inside the channel back and forth. This is much faster than publishing a transaction to the blockchain and waiting for confirmations.
When the channel is closed, the money goes back to the accounts that created it according to the final distribution of money in the channel.
[Read more about channels here](design/channels.md)


## Smart Contracts Inside Channels
Amoveo smart contracts are built on top of the Amoveo channel system. Amoveo channels allow for bets, which are Turing complete contracts written as a part of the channel.
In the case of a disagreement, the blockchain can look at the channel and its bets in order to know how to distribute the money from the channel to the participants.
[Common misconceptions about smart contracts inside of channels](design/programmable_state_channels.md)
[Read more about Amoveo smart contracts.](design/smart_contracts.md)


## Derivatives, not Subcurrencies
Since there is no on-chain state, it would be impossible to put subcurrencies onto Amoveo. Subcurrencies are contrary to Amoveo's scaling strategy.
It is also impossible to do ICOs on Amoveo, but Amoveo does support other forms of fundraising.
Amoveo has something much better than subcurrencies, it has derivatives.
With derivatives, you can build an asset that stays the same value as a Euro. It is a synthetic asset. 
You can send these synthetic-Euros to your friends, and treat them like Euros.
You could participate in a market that is priced in synthetic-Euros. 
[Read more about subcurrencies and why they are incompatible with channels here.](design/why_not_channels_with_multiple_currencies.md)


## Chalang Smart Contract VM
* [Official chalang github repository](https://github.com/zack-bitcoin/chalang)
* Used for the smart contracts in the channels.
* Chalang has two stacks for storing values during computation, so you can write highly optimized forth-style code.
* Chalang functions are tail call optimized, so you don't have to worry about call stack overflow when you do lots of recursion.
* Chalang lisp compiler is the compiler that was used to write the smart contracts currently in use on the testnet.


## Lightning Network - Smart Contracts with more than 2 Participants

Amoveo uses the hub and spoke model. Each user finds a hub to make a channel with. The user pays the hub a fee to route their payments and bets where they need to go using the lightning network.
Lightning contracts are made by hash time-locking and similar techniques. This connects multiple bets from different channels together. Either they all update, or they all do not update. This way users can participate in a smart contract that has more than two participants.

If a bet follows a long path, it could be locking up more liquidity than necessary. We can recover the excess in a trust-free way, without interrupting the bet.

For example, let's say Alice and Carol made channels with server Bob. Alice and Carol are betting against each other. Alice is betting $1 on the Astros, and Carol is betting $1 on the Cowboys. They are betting at 50-50 odds.

Alice has at least $1 locked up in her channel, and so does Carol.
Bob has at least $1 locked in Alice's channel to pay her if she wins. Bob also needs $1 locked in Carol's channel to pay her if she wins.

Alice and Carol could make a new channel directly between them, and they could trustlessly move the bet from the long path to the direct path.
This unlocks Bob's $2, so he can use them for something different.


## Markets
The killer app of blockchain is a scalable market to trade assets whose price is determined by a trust-free and affordable oracle.
[State channels are worthless without markets](design/state_channel_without_off_chain_market.md).
There is no value in being able to make smart contracts if you can't securely determine the market price for those contracts.
All the rest of the security doesn't matter if they can get away charging you above the market rate.
A market provides liquidity, so you can trade financial risk with other users at the *current market price*.
Markets on Amoveo are centralized and trust-free.
Each market exists on a centralized server.
The market is secure because the rules are enforced by channel smart contracts.
Here is an [explanation of how the market smart contract works.](design/limit_order_in_channel.md)


## Light Nodes
* Amoveo uses the stateless full node model. That means a full node doesn't have to store any consensus state to stay in sync and verify blocks. You only have to store headers. Every block has all the merkel proofs that you need to verify that block.
* this means that a full node can process blocks in any order.

## Sharding/side-chains

* You can learn about the Amoveo sharding plan here: https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md
* it is similar to the "channel factory" idea talked about in some bitcoin communities.
* basically, we are combining probabilistic payments with state channels.


## Example Use Cases
* [Fund-raising for creation of public goods](use-cases-and-ideas/insured_crowdfund.md) This is how we will fund further development of Amoveo.
* [Prediction markets](use-cases-and-ideas/prediction_market.md) This is how we will plan further development of Amoveo.
* [Insurance](use-cases-and-ideas/insurance.md)
* Gambling
* [Stablecoin, also called "synthetic assets"](use-cases-and-ideas/stablecoin.md) This way you can own US dollars on Amoveo.
* [Preventing nuclear disaster](use-cases-and-ideas/north_korea.md)
* [Preventing sexual abuse in the workplace](use-cases-and-ideas/Harvey_Weinstein.md)
* [Options](use-cases-and-ideas/options.md)


## Oracle Game Theory
*warning, this math is intense, feel free to skip this section.*
Users can bet at 50-50 odds on which of the 3 outcomes they think will win.
Whichever outcome gets the most bets, and maintains it's lead for a long enough amount of time, wins.
If you are uncomfortable with the outcome of an oracle, simply move to a fork of the blockchain that you think is honest.
By default, nodes will go with the fork that has the most difficulty, but it is easy to manually tell your node to follow a different fork.
The developers promise to maintain the honest fork.
Letting users bet helps the situation escalate to the point where it gets the miner's attention.
Any honest individual who notices an attack on the oracle can double their money by participating in defense.
The Nash equilibrium will be for honest individuals to participate in the defense. Once enough honest individuals participate in the defense, then the oracle will catch the attention of the miners.
Yes there is a cost for the miners to keep an eye on the oracle this way, but every time an attack happens, the miners can participate in the defense, and double all their money. This should more than make up for the cost of watching the oracle for attacks.
The Nash equilibrium will be for miners to put some effort into watching the oracle for potential cheaters.
Therefore, the Nash equilibrium will be an honest oracle.
[Read more about oracles here](design/oracle.md)

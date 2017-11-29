## Amoveo White Paper

## abstract
Blockchain is a form of cryptographic database technology. The state stored in a blockchain is called consensus state. Each blockchain hosts a cryptocurrency in the consensus state. Cryptocurrencies are a form of wealth that is made secure by blockchain technology.
Amoveo is a blockchain which secures a cryptocurrency called veo.
Using Amoveo you can host a market on a server where you make money every time anyone trades.
Using veo in the markets, you can buy and sell risk in anything.
With Amoveo it is impossible for customers to steal from a market, and it is impossible for the market to steal from its customers.


## motivation
Seperating church and state is good. It makes both the church and the state less dangerous.
Combining religious convictions with a monopoly on violence was deadly. It meant that people could be easily convinced to kill you at any time, and there was no consequence to the religious leader who called for your death. 

Seperating finance and state will also be good. It will make both the state and finance less dangerous.
Combining finance with a monopoly on violence means that we can't truly trust the finance system. The people who are holding our money are able to steal our money without consequence. This is a terrible combination.
[Amoveo mission statement](design/mission_statement.md)


## block consensus
Amoveo uses Nakamoto consensus, just like bitcoin. It this system, some people act as miners. They receive a reward for producing blocks. Producing a block involves doing an expensive calculation called proof of work. The difficulty of this work is reset every 2000 blocks so that the rate of block production stays about 10 minutes per block.


## accounts
Accounts are datastructures recorded in the blockchain consensus state. Each account has a positive balance of Veo that it can spend to other accounts. Anyone with sufficient Veo to pay the fee can create new accounts. Spending from an account requires a signature from the private key that the owner of the account knows. To give someone Veo, you need to know their public key.


## channels
Publishing a transaction to a blockchain is time consuming. You need to wait for the transaction to be included in a block, and then depending on how much money is moved, you need to wait for more confirmations. Waiting for confirmations means that you wait for enough blocks to be added to the blockchain for enough security. Trying to avoid this time-consuming process as much as possible is why we use channels.

Channels are 2 party relationships recorded on the blockchain. Each channel has a finite amount of money it controls, just like an account.
Once 2 people have a channel together, they can instantly move the money inside the channel back and forth. This is much faster than publishing a transaction to the blockchain and waiting for confirmations.
When the channel is closed, the money goes back to the accounts that created it according to the final distribution of money in the channel.


## move the smart contracts onto the channels
Amoveo smart contracts are built on top of the Amoveo channel system. Amoveo channels allow for bets, which are turing complete contracts written as a part of the channel.
In the case of a disagreement, the blockchain can look at the channel and it's bets and know how to distribute the money from the channel to the participants.

Putting the smart contracts into the channels gives Amoveo many advantages

* speed - off-chain state can be updated as quickly as you can communicate with your channel partner. This is much faster than waiting for confirmations on-chain. If your partner is a server, you can use Amoveo smart contracts instantly.
* privacy - off-chain state can stay secret. As long as neither channel participant disappears or tries to cheat, they never have to publish the contract on-chain.
* scalability- parallel off-chain computation. The channel contracts are usually only processed by the 2 participants in the channel. Every pair of channel participants can be running different channel contracts simultaniously.
* scalability- parallel on-chain transaction computation. Most transactions in a block do not depend on each other. Since there is no on-chain smart contract state, it is easy to calculate which transactions depend on each other. So we can parallelize processing of transactions within the same block on the same machine.


## lightning network - smart contracts with more than 2 participants
Amoveo uses the hub and spoke model. Each user finds a hub to make a channel with. The user pays the hub a fee to route their payments and bets where they need to go using the lightning network.
Lightning contracts are made by hash time-locking and similar techniques. This connects multiple bets from different channels together, so either they all update, or they all do not update. This way users can participate in a smart contract that has more than 2 participants.

If a bet follows a long path, it could be locking up more liquidity than necessary. We can recover the excess in a trust-free way, without interupting the bet.

For example, lets say Alice and Carol made channels with server Bob. Alice and Carol are betting against each other. Alice is betting $1 on the Astros, and Carol is betting $1 on the Cowboys. They are betting at 50-50 odds.

Alice has at least $1 locked up in her channel, and so does Carol.
Bob has at least $1 locked in Alice's channel, to pay her if she wins. And Bob also needs $1 locked in Carol's channel, to pey her if she wins.

Alice and Carol could make a new channel directly between them, and they could trustlessly move the bet from the long path to the direct path.
This unlocks Bob's $2, so he can use them for something different.


## light nodes and sharding
* light nodes don't download blocks, they only download small headers, and small proofs of the blockchain state.
* each block includes all the proof needed to verify it. So, light nodes can verify blocks in any order.
* light nodes can mine, which is important for sharding.
* in bitcoin the bandwidth requirement for trust-free access to the blockchain is O((number of blocks) * (transactions per block ~= 2000) * (size of a average tx ~= 1 kb)). This is so expensive that most people cannot afford to run a secure bitcoin node. Or alternatively, you can pay a server to scan the entire UTXO set every time you want to check your balance, but this is too expensive as well, no one even made a server like this yet.
* In amoveo the computational requirement for trust-free access to the blockchain is O((number of blocks) * (size of average header ~= 200 bytes)). So, per block, Amoveo will be able to sync about 10 000 times faster than bitcoin.
* Amoveo can operate without any full nodes.
* scalability- parallel block computation. Since miners don't have to store any of the consensus state, the blockchain would still be functional under these conditions: it takes more than 10 minutes for a miner to verify a block. The blocktime is 10 minutes. Compare with bitcoin or ethereum, where security depends on the fact that the time to verify a block is significantly less than the blocktime. Ethereum tried to solve this problem with GHOST, but GHOST is only a small improvement, and it comes at the large cost of inflating ETH to pay for uncle blocks. Amoveo can do much more computation per block.


## Chalang Smart contract VM
* [official chalang github repository](https://github.com/zack-bitcoin/chalang)
* used for the smart contracts in the channels.
* chalang has 2 stacks for storing values during computation. So you can write highly optimized forth-style code.
* chalang has variables for storing values. So you can write easier to read javascript-style code.
* chalang functions are tail call optimized, so you don't have to worry about call stack overflow when you do lots of recursion.
* Chalang forth-like compiler is the compiler that was used to write the smart contracts currently in use on the testnet.


## oracles
Anyone can ask the oracle any question, if they are willing to pay the cost.
The oracle can settle in 3 states: True, False, Bad Question.
Unlike some competitors, Amoveo does not have a subcurrency to power the oracle. Because this subcurrency doesn't exist, the users of the oracle don't have to pay rent to the owners. This means Amoveo's oracle will be more affordable than competitors who's oracles depend on subcurrencies, like Augur or Bitcoin Hivemind.
The Amoveo oracle does not usually have much collatoral at stake, so the cost of launching an oracle is small. To launch an oracle, you pay just enough to provide initial liquidity to get a market going.
Amoveo's oracle has the ability to escalate its defense in response to an attack. Oracles that cannot escalate are prohibitivly expensive, or they aren't secure.
It is this ability to escalate that allows Amoveo to keep the collatoral for each oracle so low most of the time. 


## oracle game theory
*warning, this math is intense, feel free to skip this section.*
Users can bet at 50-50 odds on which of the 3 outcomes they think will win.
Whichever outcome gets the most bets, and maintains it's lead for a long enough amount of time, wins.
If you are uncomfortable with the outcome of an oracle, simply move to a fork of the blockchain that you think is honest.
By default, nodes will go with the fork that has the most difficulty, but it is easy to manually tell your node to follow a different fork.
The developers promise to maintain the honest fork.
Letting users bet helps the situation escalate to the point where it gets the miner's attention.
Any honest individual who notices an attack on the oracle can double their money by participating in defense.
The nash equilibrium will be for honest individuals to participate in the defense. Once enough honest individuals participate in the defense, then the oracle will catch the attention of the miners.
Yes there is a cost for the miners to keep an eye on the oracle this way, but every time an attack happens, the miners can participate in the defense, and double all their money. This should more than make up for the cost of watching the oracle for attacks.
The nash equilibrium will be for miners to put some effort into watching the oracle for potential cheaters.
So, the nash equilibrium will be an honest oracle.


## derivatives, not subcurrencies
Since there is no on-chain state, it would be impossible to put subcurrencies onto Amoveo. Subcurrencies are contrary to the goal of scalability.
Since there are no subcurrencies, it is also impossible to do ICOs on Amoveo, but we do allow for other types of fundraising.
Amoveo has something much better than subcurrencies, it has derivatives.
With derivatives you can build an asset that stays the same value as a Euro, it is a synthetic asset. 
You can spend these synthetic-Euros to your friends, and treat them like Euros.
You could participate in a market that is priced in synthetic-Euros. 
[I wrote more about subcurrencies and why they are incompatible with channels here.](design/why_not_channels_with_multiple_currencies.md)


## markets
The killer app of blockchain is a scalable market to trade assets who's price is determined by a trust-free and affordable oracle.
[State channels are worthless without markets](design/state_channel_without_off_chain_market.md).
There is no value in being able to make smart contracts if you can't securely determine the market price for those contracts.
All the rest of the security doesn't matter if they can get away charging you above the market rate.
A market provides liquidity so you can trade financial risk with other users at the *current market price*.
Markets on Amoveo are centralized and trust-free.
Each market exists on a centralized server.
The market is secure because the rules are enforced by channel smart contracts.
Here is an [explanation of how the market smart contract works.](docs/design/limit_order_in_channel.md)


## examples use cases
* [fund-raising for creation of public goods](use-cases-and-ideas/insured_crowdfund.md) This is how we will fund further development of Amoveo.
* [prediction markets](use-cases-and-ideas/prediction_market.md) This is how we will plan further development of Amoveo.
* [insurance](use-cases-and-ideas/insurance.md)
* gambling
* [stablecoin, also called "synthetic assets"](use-cases-and-ideas/stablecoin.md) This way you can own US dollars on Amoveo.
* [preventing nuclear disaster](use-cases-and-ideas/north_korea.md)
* [preventing sexual abuse in the workplace](use-cases-and-ideas/Harvey_Weinstein.md)
* [options](use-cases-and-ideas/options.md)


## governance
Using the oracle, the parameters that define the system can be modified. The mechanism is built with the goal that the oracle will choose parameters that make the value of VEO increase.

Here are all the parameters that can be changed:
* block_reward - miner gets this reward when they find a block.
* developer_reward - developers get this reward when a block is mined.
* time_gas - this is a limit on how many CPU cycles a smart contract can use.
* space_gas - this is a limit on how much ram a smart contract can use.
* fun_limit - this is how many functions a smart contract can define.
* var_limit - this is how many variables a smart contract can define.
* oracle_initial_liquidity - the cost to launch an oracle.
* minimum_oracle_time
* maximum_oracle_time
* maximum_question_size - how many bytes big can a question for the oracle be?
* block_time_after_median 
* channel_closed_time
* question_delay
* governance_delay
* governance_change_limit - this is a limit on how quickly you can modify the governance variables.
* fees for each of the different transaction types.

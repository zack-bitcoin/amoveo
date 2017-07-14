How do state channels work?
You can read this document I helped create: http://www.altheamesh.com/blog/universal-payment-channels/
Also, Jeff Coleman has a good explanation: http://www.jeffcoleman.ca/state-channels/

Each channel is capable of doing any type of smart contract.
A typical user will have only one channel, and they will do all their smart contracts through the same channel.

Only the 2 participants of a channel keep a record of the channel state, they can update it as many times as they want.

How does the consensus mechanism of state channel works?
How do miners distinguish the true contract in the case of disagreement?
When you process the smart contract, it outputs 3 numbers:
How much money goes to the first party,
How much money goes to the second party,
And a nonce.
If both parties disagree about the final state, the blockchain prefers the state that has a higher nonce.
So every time we make a payment in the channel, we increment the nonce to be one higher.

will the miners, who don't reach a consensus with the majority be punished?
The blockchain uses proof of work mining, similar to bitcoin. The incentive is to mine on the longest valid chain.
If you mine on a shorter chain, then you probably wont get a block reward.

Will the counterparty, who doesn't execute the contract be punished?
The smart contract is turing complete. you can program it to punish users for failing to participate properly.

How long will a dispute be solved?
The state channel is programmable, you can decide how long you want the dispute period to last.


How does the oracle consensus work? How can the oracle answer all kinds of questions?
here are several documents describing the oracle:

[oracle motivations](oracle_motivations.md)

[oracle](oracle_simple.md)

[oracle design](oracle_design.md)

The oracle does need to lock money. We can use dominant assurance contracts, which I like to call "insured crowdfunding" to raise money to run the oracle.
An oracle is a public good, raising money for public goods is usually done in one of 2 ways: either by charity or taxation.
The problem with charity is that people tend to pay less money than how much the good would benefit them.
The problem with taxation is that people tend to pay more money than how much the good would benefit them.
Insured crowdfunding is a contract designed so that each person is incentivized to give as much money to the good as how much the good benefits them.

How do I solve the problem were if all the miners just follow the first one who give the answer?
Miners don't answer these questions at all. It is a market based oracle. The price of the market determines the outcome of the oracle.
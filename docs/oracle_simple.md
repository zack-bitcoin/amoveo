A simple way to do the oracle is like this:
Every time a decision resolves, the blockchain forks. One side decides that the decision's outcome is "true", the other decides "false".
Users know the truth. They prefer the chain that is more honest. So the coins on the honest chain are the ones that are valuable. So miners are only able to profitably mine on the honest chain.

This simple oracle mechanism has a big drawback.
There is a large cost to having the users manually answer each oracle decision while downloading the blockchain.
An attacker could ask lots of questions, and waste our time answering them.

So, we need to adjust the mechanism so that if an attacker makes us manually answer a question, the cost to the attacker exceeds the damage inflicted on the network.

The way to make oracle spam expensive is to use a market that measures the correlation between the answer to the question and the blockchain's difficulty.
The blockchain's difficulty is averaged over a long-enough period of time, that miners can't afford to mine at a loss long enough to manipulate the result.
Miner's profit margin is as small as possible, so user's demand for new coins determine the difficulty, not the miners.

If an attacker wants to spam the blockchain with questions, the attacker will also have to make large losing bets in the market. These losing bets cover the cost of other users having to manually answer the question.

There is still one case where an attacker can make the oracle lie.
The attacker commits to buying up enough coins on the lying fork, so that the difficulty of the lying fork is higher than the difficulty of the honest fork.
A blockchain with higher difficulty is more valuable.
So the result of this attack is a blockchain that is more valuable.
Making the blockchain valuable is more important than making the oracle honest.
So in this situation, it is acceptable for the oracle to lie.

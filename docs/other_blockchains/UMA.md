Review of UMA DVM oracle whitepaper july 2019
============

Link to the paper being reviewed: https://drive.google.com/file/d/1OfdCsaIf5tCDEMS0nnPjF7-QidY1h0hJ/view

UMA is a votecoin based oracle, very similar to Augur or Bitcoin Hivemind.

Just like Augur and Bitcoin Hivemind, they set the fee of using the oracle to try and maintain the market cap of votecoins being greater than the total value of bets being enforced by the oracle.

If there are too many bets and not enough votecoins, the cost of using UMA's oracle increases, to reward people for locking up more value in votecoins.
If there are few bets and many votecoins, then the cost of UMA's oracle decreases, to try and be more cost competitive vs the other oracle alternatives, and attract customers back.

The volatility in the value of an altcoin increases if the market cap is lower. So small cap votecoin are very volatile.
The cost you need to pay to convince someone to lock up value in an asset is proportional to the volatility of that asset.
This means we will need to pay high fees to the votecoin holders of systems like UMA/Augur/Hivemind.

Votecoin type oracles like UMA will always be orders of magnitude more expensive than Amoveo's oracle.


UMA's definition of the oracle problem:
================

"Oracles can be conceptually decomposed into two functions: a mechanism for reporting off-chain data on to the blockchain, and a mechanism for verifying the accuracy of the reported information. In most smart contract designs, if the contract participants agree on the data provided by the reporting mechanism, there is no need for verification. The verification mechanism is only required in the event of a dispute. This verification system is the focus of this paper"


So from the start they have set themselves up with limitations so that only 3.2 level oracle systems will be possible for them. You can read about security levels here: https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md
By refusing to use escalation mechanisms to smoothly transition from the reporting step to the verifying step, it is impossible for them to come up with anything better than 3.2, like Augur or Bitcoin Hivemind are.


UMA makes a claim of novel contribution to research:
==================

"Despite a large body of existing research into oracle system design, current approaches are missing one key feature: an economic guarantee around the cost of corrupting an oracle system."

Look at page 27 of the truthcoin whitepaper: http://bitcoinhivemind.com/papers/truthcoin-whitepaper.pdf
You can see a calculation of the the economic guarantees around the cost of corrupting the oracle system. This calculation has existed in this white paper since at least 2014 when I first read it.

Search for "maximum benefit to an attacker" in the augur white paper https://www.augur.net/whitepaper.pdf

UMA cite's Truthcoin, Augur, and Amoveo as having "heavily influenced UMA’s design." And yet UMA doesn't realize that all 3 of these projects spend a lot of time calculating the cost of corrupting the oracle system and coming up with economic guarantees about this.



False assumption about mechanism design
=================

One of the biggest false assumptions that UMA is building on is captured in this quote:

"Since behavior on a permissionless blockchain is only
motivated by economic incentives, there exists some bribe that can corrupt the verification mechanism and produce dishonest behavior."

We already have substantial research in mechanism design theory to know that this is false. There are economic mechanisms where any attempt at bribery can only serve to make the mechanism more accurate http://mason.gmu.edu/~rhanson/biashelp.pdf

The trick is to set up the report as a bet, so that false reports act as a prize to incentize others to make honest reports. You can use the attacker's momentum against them. This allows the amount of security to escalate in proportion to the size of the attack, so we are always just secure enough.


UMA's offensive description of Amoveo:
================

"Prediction market systems like Truthcoin, Augur and Amoveo have made important contributions to verification mechanism design; these systems all use Schelling Point voting schemes to incentivize truthful voting and have heavily influenced UMA’s design."

Amoveo has no voting mechanism anywhere. We use futarchy and Nakamoto consensus.

Voting can't be better than 3.2, it will always be orders of magnitude more expensive than futarchy.
https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md

You can read about Amoveo's oracle design here: https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/oracle.md


Confusion about layering oracles
================

UMA is confused about basic concepts from oracle design:
"the UMA verification mechanism could be layered on top of another oracle, adding economic guarantees to the accuracy of that oracle’s reported data and allowing UMA to function as a verification layer of “last resort.”"

Wow, that sounds so socially positive. Like we are all in this together. Too bad it isn't true.

Chaining mechanisms on top of each other makes the entire system as weak as the weakest link.
For voting schemes like UMA or Augur to work, the value of the votecoins needs to exceed the value of all bets being enforced at that time.
If we layer 2 voting type protocols, then that means the total value of the votecoins now needs to be 2x bigger than the value of bets, doubling the minimum required fees to run the system.

So no, it would never make sense to layer oracles on top of each other like this. It can only make it less secure and more expensive.





Parasites and fuzzing.
=========

UMA describes the parasite problem well. It is a problem that happens to votecoin systems like Augur/Hivemind/UMA.
The parasite problem happens to them because they need to collect fees to pay to the votecoin holders to maintain security.
Oracles like Amoveo don't have the parasite problem because we don't have any votecoin holders. Our oracle does not collect fees from traders, so in Amoveo it doesn't matter if the bets are off-chain and private.


The fuzzing solution relies on a false assumption.
They are assuming that the parasite contract will be looking at how someone else's bet gets resolved, and it will mirror the other existing bet.
But a parasite contract could also be programmed to look up the result of the oracle by verifying a merkel proof of Ethereum's consensus state at a particular block height. That is how you could export data from Ethereum into other blockchains, and have off-chain parasite contracts.




Bribes and Collusion
=========

"Using the assumption that the price of tokens drops to zero if the system is successful manipulated, we can show there is no bribe an attacker can profitably pay token holders. If token holders believe their token value will drop to zero if the system is corrupted, each token holder will demand a bribe greater than their current token price p."

The size of the bribe the attacker needs is much less than the value of tokens each person is holding. This is called tragedy of the commons. I will make a counter-example to prove this is false.

Lets imagine that there are 100 token holders, each with 1% of the value. We are reporting on "is 1+1=2 today?", and the attacker is bribing the token holders to incorrectly report "false". Lets assume, as UMA did in their paper, that if the attack succeeds, then the value of the tokens goes to zero.

The attacker uses a smart contract to make this commitment to each oracle reporter: "If you participate in the attack, I will pay you 10% of the value of the tokens that you are holding."

So now the attacker has locked up 10% the value of all the tokens in a smart contract of commitments.

Each oracle reporter holds 1% of the tokens, and they have to make this decision: to participate in the attack, or not.
Lets calculate their expected profit to see which decision is more profitable:

```
M = starting market cap of tokens
r = how much a reporter has = M/100
b = a single bribe = r/10
p = probability that the attack succeeds
```

assuming that if you participate in the attack, it makes the attack 1% more likely to succeed.

```
participate in attack -> b + (1-(p+(1/100)))*r
does not participate in attack -> (1-p)*r
```

it is profitable to pariticipate in the attack if:
```
(b/r) + (1-(p+(1/100))) > 1-p
b/r > 1/100
b > r/100
```

So the reporter would profitably participate in the attack as long as the bribe is bigger than 1% of the value of their tokens.
If the attacker wants to bribe 51 of the 100 voters, he only needs to pay 0.51% of the total value of the tokens.
This is the same tragedy of the commons math as mechanism designers have known about since at least 1833. 


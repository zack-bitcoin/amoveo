Fast Oracles
========


This is review and commentary on the oracle design proposed by James Lee here: https://medium.com/@jameslee777/decentralized-trustless-oracles-dto-by-piggybacking-on-timestamp-consensus-rules-2adce34d67b6


Comparison to Amoveo's oracle
========

Amoveo's oracle has a novel design, instead of building up a second consensus mechanism to control the outcome of the oracle, it re-uses the existing blockchain's consensus mechanism to secure the oracle as well. This allows it to achieve [2.2 level security](../basics/trust_theory.md), and to be more affordable.
jl777's oracle is similar this way. It is reusing the blockchain's consensus for the oracle instead of building a second consensus mechanism to control the oracle.

For Amoveo we didn't want the miners to have to care about the oracles almost ever. We have some escallation mechanisms in place so that it is massively expensive to cause a situation where the miners would need to pay attention to an oracle. If miners did need to pay attention, it would necessarily mean that the miners can take big profits from the attacker's losses.

jl777's oracle went with the opposite extreme. Miners need to participate in looking up and recording the outcomes of all the oracles. So it doesn't need escalation mechanisms. In some ways it is a simpler more elegant design.


Nakamoto Oracles
=========

The heart of Nakamoto consensus is a game like this:
Mining pools have to run the version of software that miners want to mine, otherwise they will get no customers.
Miners have to mine the version of software that produces coins that users are willing to pay for, otherwise they can't afford electricity.
So the relative value of the tokens on each side of the fork is what controls Nakamoto consensus.
If we want to reuse Nakamoto consensus to supply more data, we need this same principle to apply. The tokens on the honest side of the fork need to be more valuable.


What I like about jl777's oracle
==========

It is great that jl777 has avoided using any known [level 4.1](../basics/trust_theory.md) or worse mechanisms.
The mechanism you create can only be as secure as the least secure tool you use to build it.

This new oracle design takes advantage of how easy it is to make new blockchains with specialized consensus in Komodo. Taking advantage of these tools from Komodo seems like a promising way to find innovations in oracle design.

jl777's design is a lot lighter. Providing a bit of data is probably less than 1/1000th as expensive as providing a bit of data in Amoveo's oracle.

I am honored that jl777 would consider reusing ideas from Amoveo's oracle in his work.


Speed concern
=========

Looking at the DAO hack, and how Ethereum recovered. It seems to me that the decision to do a hard update to prevent the money from getting stolen from the dao, this is almost identical to making the decision in jl777's oracle about what the outcome should be.

Ethereum was only able to do a hard update to prevent the DAO hack because the money being stolen was locked up for a long enough period of time. The money needed to be locked long enough for the users can develop some expectation of what the price of Eth would be on either side of a fork, if only one side of the fork let the DAO get hacked.

Nakamoto consensus can control the outcome of the Ethereum hard update, but only if it is slow enough for the users to understand what is going on and make an educated decision as to which side of the fork they want tokens on.

My concern is that jl777's oracle is too fast for Nakamoto consensus to control the outcome. There is not sufficient time for the users to make an educated decision as to which side of the fork they want tokens on.

If a decision is made so quickly that users don't have time to realize that a fork is happening, then the prices on each side of the fork wont change the way they are supposed to.
Instead of being restricted to mining on the honest version, miners would instead be restricted to mining on the popular version, even if it is dishonest. Because the tokens on a minority fork look worthless.
By the time the network realizes a lie was included in the oracle, it is too late to be fixed. The attacker has already laundered the stolen money.


Syncing concern
=========

Whatever software the full nodes are using to look up the oracle data they are recording, it isn't possible that it is deterministic across time.
Eventually API on the internet change.

This means it isn't possible to verify the accuracy of historical data that the oracle had provided.
So whatever fraction of the network is running at one time, they could work together to insert lies into the oracle.


Exception to the previous concerns
=========

If every node is running a full node, and they all stay in sync 100% of the time, then my concerns before do not hold.
Users can instantly know which side of the fork they want to control tokens on, because their full node can look up the oracle result exactly the same as every other full node.
There is no syncing issue, because no one ever resyncs any history.

It is possible that side chains that could almost instantly have access to data from anywhere on the internet, these sidechains could be very useful.
Even if they have all these restrictions:
* everyone who can participate in the blockchain is a full node
* new people can't join after it started
* everyone stays synced 100% of the time


Recommendations
=========

I see 3 options to progress this oracle 
1) put a delay in the oracle so that it can be secure on a normal blockchain.
2) prove that Nakamoto consensus is still controlling the outcome, even if things are happening faster than the user's can be aware of.
3) find some applications of the blockchain where everyone is a full node. Make tooling so it is quick and easy to form and disband these blockchains.



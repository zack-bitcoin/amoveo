Bribery
=======

Bribery in blockchains was studied by Vitalik Buterin https://blog.ethereum.org/2015/01/28/p-epsilon-attack/
He showed that voting mechanisms are vulnerable to bribery, and then he showed that POW is vulnerable to bribery the same way.

The goal of this paper is to show that bitcoin POW is not vulnerable to bribery, and that voting is vulnerable to bribery.


Security of voting-type oracles
=======

With an Oracle there are 2 outcomes. The enemy of your enemy is always your friend. There are only 2 mutually exclusive ways to participate.

Besides the attacker who is paying bribes to get the voters to participate in the attack, defenders can also pay bribes to get voters to participate in defense.

According to tragedy of the commons, the attack succeeds if ((attacker bribes) - (defender bribes)) > ((value of the votecoins) / (number of votecoins holders))

Oracles are different from blockchain consensus
==========

The oracle problem and blockchain consensus problem have a lot in common, and because of that we have a tendency to re-use tools from one in the other. But, they have key differences.

In pow, there are infinite ways to build the next block. 
So if you are part of an attacker majority, you can form a smaller coalition with majority control of the bigger coalition, and take control of block production.
In pow, you can't trust anyone making a coalition, because the game is inside of itself recursively. And it is always easier to build a coalition inside of an existing dominant coalition, in comparison to how hard it was to build the existing dominant coalition.

In POW, no one knows how many layers of soft forks there are.
Even if a miner accepts your bribe, and complies with your conditions, it still might not achieve the goal you had in mind. Because there could be another soft fork on top of your soft fork, created by a majority coalition of the attackers.
In POW, you can't bribe miners to control the consensus, because you can't know all the rules involved in consensus, because you can't know how many soft forks there are.

So bitcoin is not vulnerable to bribery the way that oracles are.


Conclusions
=======

The computation on page 27 of the bitcoin hivemind whitepaper is wrong: http://bitcoinhivemind.com/papers/truthcoin-whitepaper.pdf
The actual cost to bribe the votecoin holders is about (1/(number of votecoin holders)) less than what his math is showing.
This puts Bitcoin Hivemind and UMA's oracles firmly in the 4.2 security level. You can read about security levels here https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md

A centralized prediction markets can at least hire lawyers and try to survive. Bitcoin Hivemind is likely less secure than centralized alternatives.

Augur has a mechanism so that their votecoin subcurrency, called Rep, can be split into 2 child subcurrencies, one of which is honest. In this way they can punish the oracle voters who reported wrongly.
Assuming that conditional bribery does not work, and that parasite contracts don't exist, which are the same assumptions made when evaluating Bitcoin Hivemind, then Augur is 3.2 level secure.

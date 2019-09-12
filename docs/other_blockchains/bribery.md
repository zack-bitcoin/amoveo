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


Conclusions for Bitcoin Hivemind
=======

The computation on page 27 of the bitcoin hivemind whitepaper is wrong: http://bitcoinhivemind.com/papers/truthcoin-whitepaper.pdf
The actual cost to bribe the votecoin holders is about (1/(number of votecoin holders)) less than what his math is showing.
This puts Bitcoin Hivemind and UMA's oracles firmly in the 4.2 security level. You can read about security levels here https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md

A centralized prediction markets can at least hire lawyers and try to survive. Bitcoin Hivemind is likely less secure than centralized alternatives.

Augur has a mechanism so that their votecoin subcurrency, called Rep, can be split into 2 child subcurrencies, one of which is honest. In this way they can punish the oracle voters who reported wrongly.
Assuming that conditional bribery does not work, and that parasite contracts don't exist, which are the same assumptions made when evaluating Bitcoin Hivemind, then Augur is 3.2 level secure.


Limitations on voting mechanisms
=========

Now we have tighter restrictions on when a voting mechanism can achieve level 3 trust.
* the voting reputation needs to be tradable as a subcurrency, I will call it "votecoins".
* the value of the votecoin needs to always be more than some constant factor larger than the value of all the money at stake in the outcome of the vote's decision at any one time.
* if the majority of voters make a decision that the community dislikes, there needs to be a way to fork the subcurrency to punish everyone who voted in a way that the community disapproved of.

If all these conditions are met, it is possible for a voting mechanism to achieve level 3 trust.
If any condition is not met, then your voting mechanism has level 4 trust, and it is likely to be less secure and more costly than centralized alternatives.
Meeting all these conditions means that it is very expensive to develop and update a voting mechanism.

It doesn't make sense to use voting mechanisms, since alternative mechanisms designs like nakamoto consensus can achieve level 2 trust, and so are orders of magnitude less expensive than voting could ever me.


Limitations on Proof of Stake blockchain consensus
=========

In order to achieve level 2 trust like Nakamoto consensus, or level 3 trust like Augur, a proof of stake mechanism needs to behave the way pow or augur does under bribery. If it behaves like binary voting, then it is vulnerable to cheap bribes because of tragedy of the commons.

If you try to bribe pow miners to form a coalition and take control of block consensus, you make yourself vulnerable to a smaller sub-coalition that will take control from you.
The smaller sub-coalition is bribing half as many miners as you, so it costs them half as much to pay the bribes. And they are spliting the rewards between half as many miners, so it is twice as profitable.
So it is a lot easier to build the sub-coalition than the coalition.
This fact makes everyone suspicious of any coalitions at all, it is just a trick to steal your hashpower.

PoW is a very splintery sort of consensus, the critical property is that you can completely conform to a coalition and a sub-coalition's demands simultaniously. Which is the same thing as saying that it needs to be impossible to punish censorship. 

If it is possible to punish the consensus mechanisms participants for failure to participate in some way, then that means the first layer of coalitions can punish their participants for failure to participate in the first coalition, which means they can punish anyone for forming a sub-coalition.

This problem persists even if the original PoS mechanisms does not punish anyone for failure to participate. By merely connecting stake owned on the blockchain to the decisions made by the consensus mechanism, you are opening yourself to the vulnerability that attackers could confiscate or destroy that stake to enforce their coalition.

So, if your proof of stake mechanisms involves having some value in the consensus mechanism, and your influence over which blocks are selected is proportional to how much value you have, this kind of mechanism can never be better than level 4 secure. It is probably less secure than a centralized alternative.

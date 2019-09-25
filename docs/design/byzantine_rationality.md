Byzantine Rationality
==========

There are 2 popular security models in cryptocurrency: Byzantine fault tolerant (BFT) modelling, and rational modelling.

BFT modelling comes from theory of distributed database design, which is a part of computer science. 
BFT modelling is based off the assumption that your database has N nodes, and that attackers have modified the code on less than 1/3rd of your database nodes.
We can only use BFT modelling for systems where we are certain that >2/3rds of the nodes will always run the default version of the software.

Rational modelling comes from game theory, and the theory of evolution.
Rational modelling is based off the assumption that users prefer owning more value instead of less.
Over time, people who have a more rational strategy will end up owning a larger portion of the value in the system. So the rationality assumption becomes more accurate as the system evolves.

Why BFT modelling does not work for blockchain
==========

A proof can only be as strong as the assumptions it is based upon.

BFT's basic assumption, that >2/3rds will run the default software, this is not a valid assumption in the context of a blockchain.

For example, if I offer >1/3rd of the nodes bribes to run slightly different software, then I have broken the BFT assumption.
If the stake is well distributed, then these bribes are cheap according to [tragedy of the commons](https://www.youtube.com/watch?v=DsdsxQqZPmA)

What motivates people to want to use BFT modelling in the context of cryptocurrency?
==============

BFT modelling makes blockchain engineering very easy.
If we can assume that >2/3rds of the stake is owned by nodes that only produce blocks that obey the default strategy, then it is very easy to make systems that are provably impossible to break.

BFT modelled systems are "provably secure".
And all their proofs are 100% accurate, no one can find a flaw in the math.
But it is all worthless, since it is built on a false assumption.

Scammers want you to believe in BFT because then they can trick you into investing into their broken systems.

Response to critics
==========

Here is a paper where someone tried to attack rational modelling in the context of blockchains.
https://bford.info/2019/09/23/rational/

This paper is based on the logic similar to Godel's incompleteness theorem.
Godel's incompleteness theorem basically says that in any system of logic, there will exist statements that are true and cannot be proven to be true from within the system.

To show that this kind of metacircular logic does not work in this context, it is sufficient to show that the users running full-nodes, they can make decisions about what code to run based off of information external to the blockchain.

And we already have abundant evidence that users can make decisions about what software to run based off of info external to the blockchain.

* all blockchain oracle systems are designed specifically to import external information inside the blockchain. 

* Futarchy type mechanisms allow for participants of a blockchain to make decisions about how to do hard updates as a community, and they can make these decisions based on info external to the blockchain. [video on futarchy](https://www.youtube.com/watch?v=higdjijPP1s)

* In the DAO hack, ethereum was able to recover because the money was locked up for such a long period of time, the community was able to come to agreement to do a hard update to prevent the theft. We can design a blockchain to lock up money for long enough periods of time to allow the network the opportunity to do a hard update. The hard update can be designed based on information external to the blockchain, like the existence of bribes or an attack.

So metacircular arguments do not work against blockchains. The users running nodes are not blind machines running the code they were given. They are rational actors capable of doing independent research to earn more profit.




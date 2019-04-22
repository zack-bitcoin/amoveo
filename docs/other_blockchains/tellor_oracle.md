Tellor Oracle
=========

Tellor oracle is an idea for a smart contract to put on ethereum that would act as an oracle.
https://docs.wixstatic.com/ugd/778e80_4230ce4c9f4a48f5ab3f06db2759f222.pdf



Some problems with the design of this project:
* The tips system makes this oracle worthless. Users can't know which questions the oracle will answer until it has already answered them. So it isn't possible to make a contract that references an oracle, unless the result of the oracle has already been recorded on-chain. This means the oracle is worthless. You can ask the oracle who won a football game, after the football game is already over. But at that point it is too late to make any bets.
* putting it to a vote by all Tellor holders is not a secure mechanism. Voting is vulnerable to bribery because of tragedy of the commons. https://vitalik.ca/general/2019/04/03/collusion.html  https://blog.ethereum.org/2015/01/28/p-epsilon-attack/
* taking a security deposit from the people who determine the result of the oracle does not make it more secure. Since it is cheap to bribe the Tellor holders, this security deposit would not get confiscated during an attack. It is a meaningless security deposit.
* rewarding the median does not make it more secure. During an attack we would be rewarding the median of the attackers, and the honest reporters would not get rewarded at all.


Voting does not work in blockchains
==========

1) the nash equilibrium of voting is rational ignorance
2) it impossible to prevent selling votes
3) P+epsilon attack means an attacker can cause us to vote for something we hate, and the attacker doesn't even have to pay us to do it.


1
The individual cost of being an informed voter is high. The individual benefit of being an informed voter is almost zero. So the rational voter does not waste time becoming an informed voter. The ration voter votes from the position of ignorance.

2
Under Nakamoto security it is not possible to prevent people from proving how they voted. If they can prove how they voted, then it is also possible for an attacker to use a smart contract to commit to paying a bribes to influence voters.
Under Nakamoto consensus it needs to be possible for the miners to calculate the consensus state of the blockchain after including the next transactions.
The miners need the ability to either include or not include any valid txs. Otherwise someone could prevent mining by refusing to share a tx.
So, the miners can calculate the next block's state after censoring any of the txs from that block.
Under nakamoto consensus everyone needs to be able ot mine, so everyone can calculate the next block's state after censoring any of the txs.
Since votes are txs, that means we can calculate the consensus state after removing any votes from the pool of votes.
By looking at the result of the election after censoring various votes, we can calculate each individual's vote.

3
The P+epsilon attack is where an attacker commits to paying a bribe, but the attacker only has to pay if the attack fails.
The nash equilibrium is for the attack to succeed, and so the attacker don't have to pay any bribes.
You can learn more here:
https://blog.ethereum.org/2015/01/28/p-epsilon-attack/

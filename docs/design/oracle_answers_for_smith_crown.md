The piece that still confuses us is the consensus process involving oracles--in particular, determining who is right between two counterclaims about who won the election.
It is a market. Everybody can participate and do "claims", which move the price (corresponding to the likelyhood correlating to the diffifculty)

It is a market. Everyone can bet in this market. There are no "claims" or "counter-claims".


The white paper states "Any aeon-holder can launch an oracle by committing to answering a yes/no- question...once the oracle launcher has supplied an answer or until a certain amount of time has passed, any other users can submit counter-claims by depositing the same amount of aeon...If any counter-claims are submitted, then the consensus mechanism for blocks will be used to answer the oracle."
 This seems different than opening an oracle by betting on an outcome and having others bet against you if they disagree, unless we are seriously misreading one of these.

This section of the white paper is very outdated, we have learned much since writing it.

Is the consensus process for determining the answer to a disputed oracle similar to augur's, in that some form of referee (like a rep holder) gets to serve as the source of verification? If so, how is that referee chosen?

Augur's oracle is a voting-based oracle consensus mechanism.
The Aeternity oracle consensus mechanism is market based.

A drawback of voting based mechanisms is that the cost of bribing a voter to vote maliciously decreases by the square of the portion of coins they own. If 100 people each own 1%, then the cost to bribe them all to vote maliciously is 1% of the coins. This is called tragedy of the commons.
Augur also uses a subcurrency. A drawback of subcurrencies is that the incentive to vote correctly is limited by the market cap of the subcurrency. So for the oracle to be accurate, the market cap of the subcurrency needs to be higher than the volume of open bets. Keeping the market cap of the subcurrency so high means that we would need to pay the holders of the subcurrency a lot of money, which is very expensive. Market based oracles are much more affordable.

The result of the aeternity oracle is based on the prices in a market.

If this is still to be defined more precisely, that's fine, just let us know. If we're missing something obvious, also please let us knoew.
We aren't technical crypto-geniuses, but neither are the investors who read our research. If we don't understand, others certainly won't and we won't be able to explain other than to say "they claim it works and they have a smart team, but we're not sure how." That could be enough for you but generally we want to represent good thinking accurately.

ok, ill define it more precisely:
    The oracle market will be a on-chain LMSR (logarithmic market scoring rule) order book with 6 possible outcomes:
 1) high difficulty, oracle decision is yes
 2) high difficulty, oracle decision is no
 3) high difficulty, oracle decision is undefined
 4) low difficulty, oracle decision is yes
 5) low difficulty, oracle decision is no
 6) low difficulty, oracle decision is undefined
   The initial liquidity for the market will be provided by a dominant assurance contract (also called "insured crowdfunding") which you can read about on page 14 of this: http://bitcoinhivemind.com/papers/3_PM_Applications.pdf
   The oracle's decision will be made if the market's price stays close to one decision for a sufficient amount of time.


Lets consider the theory behind why this mechanism should work:
    Imagine the blockchain split in a fork, one side recorded the answer to the oracle honestly, and the other side lied. It is my expectation that the honest side of the fork will have more valuable tokens because users would prefer to own tokens on a blockchain that has an accurate oracle.

Now, lets consider a couple attacks:
    What if an attacker makes bets in the market to manipulate the price and get the oracle to lie?
        > Any price manipulation acts as a prize for honest traders to come and put the price where is belongs. Research shows that efforts to manipulate prediction markets actually make them more accurate.

    What if the attackers not only manipulate the market, but also censor the honest traders, so the price stays wrong?
        > Miners can only censor traders if they control >50% of all the mining power. If this happened, then our security assumptions are broken, and the blockchain is insecure in many ways.


Invalid Oracles
=========

There is an interesting attack that has been happening to the Augur blockchain.
Attackers ask the oracle questions with the intention of having the result be ambiguous, but at first glance it looks like the oracle is not ambiguous.
The attacker makes bets on the less likely outcome, and so they buy shares at a price <0.5.
Once the oracle results in "bad", then all the money in the market is split 50/50 between the people who bet on either outcome.
So the attacker who paid <0.5 now gets paid 0.5. So this is a profitable attack.

One solution to an attack like this would be if everyone got paid back the same amount of money that they had bet with. That way this attack could not result in theft.
This design choice would mean that the shares in the market are no longer fungible. They have a different value depending on the price at which they were purchased and the likelyhood that the oracle will be invalid.
So this design is incompatible with subcurrencies, it could not be an ERC20 on Ethereum.
But it could be built inside the Amoveo lightning network.
This design choice means that channel hubs making markets need to be careful not to use oracle that might result in "bad". Because the channel hub could lose a lot of money in that case. If the different bets are not fungible, then arbitrage stops working.
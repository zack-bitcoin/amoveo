What if someone payed money into the market that we use as an oracle to try to get it to lie?

Example: we were betting on a football game, but now the game is over and we all know that Alice won.
The correct outcome is that decision A is 99.99% likely to be the fork of the blockchain with high difficulty, and decision B is 0.01% likely to be the fork with high difficulty.
An attacker keeps buying B to push the price above 50%.
Defenders show up to buy shares of A at one-to-one odds.
The attackers own lots of tokens on fork A, and update their software to use fork A.
The defenders own lots of tokens on fork B, and update their software to use fork B.
Everyone else prefers the fork that is honest, because they want a fork with a working oracle.
It is the shelling point.

When the price is 50-50, the act of defending has this return rate:

`1-(interest_rate*finality)`

Where interest_rate is the market rate you need to pay someone to be willing to hold Aeons for a time and be unable to access them.
finality is the amount of time until the bets pay off.
Defenders will only defend if they earn a profit, so, the oracle is only secure when

`(interest_rate*finality) < 1` .


Blockchains are small and unstable, so the interest rate should be a very high amount of 10% per month, which is about 0.3% per day.

The amount of time until bets can pay off needs to be longer than the difficulty retargetting period.
That way the blockchain can know the demand for coins.
In bitcoin, the retargetting period is 2000 blocks, which is about a week.
So lets set our finality to 7 days = about 2.5% interest

Plugging in to the formula:

`(0.025 * 1) < 1`

Which is true. So it is secure.




Besides using markets for telling us true/false binary things, we also ask the oracle questions with less certainty. For example: "If we double the size of blocks, will that make the coins more or less valuable?"
Depending on the price, we update the protocol.
How valuable does a change need to be for us to be able to measure it?

Lets say the price should be X, which is between 0.5 and 0.
An attacker keeps pushing the price to 0.51, so change the outcome and force the protocol to update how he likes.
The defenders profit for defending is

`1 - (2 * X) - (interest_rate*finality)`

`= 1 - (2 * X) - 0.025`

The defender will only defend if this is positive, so

`0.975 - 2X > 0`

`X < 0.975 / 2`

`X < 0.4875`

So the attacker can only push the price a maximum of `(interest_rate*finality)/2`
If he pushes more than that, then it becomes profitable to defend against the manipulation.

If we only have < 2% confidence that decision A is better than B, then it isn't important which decision we make. It doesn't matter if we cannot prevent such tiny manipulations.

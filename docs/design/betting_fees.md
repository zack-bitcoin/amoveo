This page estimates the cost of betting, and compares it to Augur, a popular blockchain prediction market.

The cost of trading in Amoveo is because you have to pay a hub to match your trade with someone elses.
With Amoveo we can trustlessly move a bet from the indirect path through the server to a direct path between the 2 gamblers. This frees up the hub's money, which had been used to make the bet in the first place. So the server's money is only locked up for unmatched bets sitting in the order book.
The cost is (interest rate of VEO) * (how much veo the server has to lock up for your bet to be enforced) * (how long your open bet sits on the server waiting to be matched).

For example, if you are betting $100 in a market with 50-50 odds, and it takes 1 hour for the bet to be matched, and the interest rate of Veo is 10% per month, and it takes 2 months for the bet to be settled, then the cost of making this bet would be
24 hours/day * 30 days/month = 720 hours per month
1.1 is interest rate per month.
X**720 = 1.1
log(X) = log(1.1)/720
X = 1.00001324 is interest rate per hour

It would cost less than 2 cents for a bet like this.
$0.01324

After the bet is matched, then the bet is moved from the indirect path through a hub to a direct path between the 2 parties who are betting. This unlocks liquidity on the hub, which makes the betting process hundreds of times more affordable.



The cost in Augur happens because the market cap of Rep needs to stay high. The total volume of unsettled bets needs to be less than 1/7.5 the market cap. The fees being paid to the holders of Rep need to be enough to convince people to hold Rep.

(7.5x is from page 9 of the Augur white paper. II.A.4 - Incentives and security - integrity of the forking protocol - integrity.)

Next looking at the cost of using Augur. The cost per bet is (the interest rate of Rep) * (the value you gamble with) * (The market cap of Rep) / (Current total volume in rep of unsettled bets) * (How long until the bet can be settled)

We will plug in the same numbers as used in the Amoveo example, to compare the cost.

(the market cap of Rep) / (Current total volume in rep of unsettled bets) is always > 7.5. To be generous to Augur, we will assume the best case scenario, and set it = 7.5

X = (0.1) * ($100) * 7.5 * 2 = $150

So with Augur, it would cost $150 to make the same bet under the same conditions.

If Augur uses smaller fees than this, then they will not be able to pay the Rep holders enough, which will cause the market cap of Rep to decline.
The total volume of unsettled bets needs to be (market cap of Rep)/7.5, so when the market cap decreases it also decreases the utility of Augur.

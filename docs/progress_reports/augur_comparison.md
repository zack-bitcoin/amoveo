Comparing Amoveo with Augur
=============


Augur's oracle has a limit. The total amount of money at stake in betting needs to be significantly less than the total value of Rep.
With Augur's design, Rep needs to always be worth 7x more than the total money in all the bets, so on average Rep will be worth around 14x more than outstanding interest. Assuming around a 15% interest rate in cryptocurrency investments, annually fees on bets will be 14*15 = 210%, and for a month fees will be 6%

I wrote the MVP for Augur.

I am involved with another blockchain called Amoveo, we solved the limit in the oracle. Amoveo's oracle can support any amount of betting, and we have zero fees for P2P betting. Fees for market approach zero, because the cost of running a market is a small flat of renting a server, no matter how many people want to participate, and anyone can run a market.
This assumes you stick around online long enough to optimize your channel to a direct path. If you do not do this, then the fee is the interest rate of the money your are betting, which we already assumed is 15%.
15% is Amoveo's worst case annual fees, it is less than 1/10th as expensive Augur's average case annual fees of 210%.
Amoveo allows anyone to ask the oracle questions, which gives us more things to bet on. You can do it from this browser interface http://139.59.144.76:8080/new_oracle.html

If you plug in any other number besides 15% annual interest rate, you will see that Amoveo's worst case is still 14x cheaper than Augur's expected case.
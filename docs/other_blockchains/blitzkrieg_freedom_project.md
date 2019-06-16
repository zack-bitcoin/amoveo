Economics of Blitzkrieg Payments
=========

Blitzkrieg is an interesting project because they use a 4.1 level security mechanism, but they keep the portion of money locked inside the 4.1 part of the mechanism very small. The vast majority of the funds are in 2.2 level security parts of the blockchain.

The goal of this paper is to find out how effective this trick can be, and when it makes sense to use it. And to compare this design vs an alternative where we would use a level 2.2 on-chain entropy generator.

The costs we want to measure are:
* cost due to risk in probabilistic payments. You could get unlucky and lose a lot of money. Variance cost.
* cost due to trusting the finder. Finder's fee.

Variance Cost
=======

How much do we have to charge per probabilistic payment.

The channel hubs need to keep enough liquid capital to continue providing their service. So they want to have mathematical certainty that losing more than N portion of their money in a single month is something that happens less than M portion of the months.

If the individual payments are more or less the same size

What we know:
N we can lose this portion of money less than ...
M portion of months.
P payments per month.
A average amount sent.


Prob(I,N,P) ->
%I is starting balance
%N is goal balance
%P probability that you increment instead of decrement.
    X = (1-P)/P,
    (1 - (X^I)) / (1 - (X^N)). %probability of getting rich

Prob(



What we want to know:
Q What portion of our money should we lock into a probabilistic payment account?
F How much fee should we charge per tx


Finder's Fee
=======

The finder could run off with the money in a single payment.
So their expected profit from fees from all their payments in the future needs to exceed the expected cost of stealing the money in this single payment.

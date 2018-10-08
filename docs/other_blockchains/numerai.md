Numerai
========


Numerai is a blockchain for selling information.
Amoveo is a blockchain for financial derivatives, which are the optimal kind of financial contract for selling information.
So Numerai is try to accomplish a subset of Amoveo's goals.
This means we should study each other's work.

Numerai is different from other blockchain finance projects in that it doesn't depend on an oracle. 

Usually if Bob wants to sell information, he do this by making a trade in some market.
For example, by buying leveraged options.

Numerai proposes a new mechanism for selling information.
First Bob needs to build up a reputation by giving away information for free.
Then Bob takes the info feed private, and sells access to it.
There is a "griefing factor" for each private feed.
Bob chooses his own griefing factor to balance attract customers and taking profit.
If the griefing factor is 1:5, that means that anyone who burns $1 can cause $5 of Bob's money to be burned.

[long blog post where I learned about numerai](https://medium.com/numerai/numerai-reveals-erasure-unstoppable-peer-to-peer-data-feeds-4fbb8d92820a)

Summary:
The information you can buy from Numerai is less accurate than what you could find out for free by looking at the price of a market for derivatives.
If you have information to sell, you could get a better price by selling it using leveraged derivatives instead of Numerai.


Problems with Numerai, table of contents--
======

1) Numerai ignores thousands of years of study about the optimal way to sell information
2) sybil attacks
3) retirement attacks
4) griefing tool is the definition of a vulnerability
5) calculating how big the grief deposit needs to be
6) calculating whether it is more profitable to sell information in a normal market, or in Numerai
7) Numerai cannot be used if you only have 1 secret to sell.
8) Parasite contracts.
9) Info feeds don't aggregate data the way that markets do.
10) People who want to buy information don't know how big the griefing security deposit should be.

.

1) Numerai ignores thousands of years of study about the optimal way to sell information

Humans have been making small improvements to market technology for thousands of years. They are highly optimized for people to sell information.

2) sybil attacks

A sybil attack is when you make many different accounts, do risky strategies with all of them, and then try to profit off the ones that were lucky out of randomness.
Numerai claims that the griefing tool prevents this problem, but that is only true in some cases.
In some cases it is profitable to burn the grief money to trick someone into making the wrong trade in some big market.

3) retirement attacks

Numerai depends on building up some non-spendable reputation.
In blockchains, non-spendable reputation is usually insecure because it enables retirement attacks.
That is when someone wants to stop participating in Numerai, but they don't want to waste the value of their reputation they built up.
So they might as well use the reputation to participate in an attack on Numerai, that way they have some chance of getting some more value out.

4) griefing tool is the definition of a vulnerability

Usually if a griefing mechanism like this accidentally appears as a side effect in any protocol, we consider it a mistake. We would redesign the protocol to remove this flaw.
If it is possible to burn $2 from you by only spending $1, there are some people who would do it.
Competitors selling information might attack each other to control the market.
Competing blockchain could do this to sabatoge Numerai as a competitor.
If you can get rid of everyone selling honest information, then it becomes easier to trick people into trusting lying information.

5) calculating how big the grief deposit needs to be

Imagine the prediction is that the price of some stock will go up %5, the person making this prediction owns $100 million of the stock, and they want to sell. They are making a lying prediction to try and sell at a better price.

So the lie lets them sell for an additional $5 million.
As long as the griefing deposit is < $5 million, it is profitable to lie.

Requiring a $5 million deposit in order to sell information on Numerai is unreasonably expensive, especially when their market cap is $5 million. And $5 million griefing deposit wouldn't be sufficient in many cases.

In general, the griefing deposit needs to be bigger than the total amount of money a person could gain by tricking us by making the oracle lie. This is usually such a large amount of money as to make selling information on Numerai impossible. No one can afford to make a safety deposit that big. Especially in a volatile altcoin, it is unreasonable to require users to buy up so many coins and make massive safety deposits. If the coins are volatile, then the interest rate will be high, so the cost of the safety deposits is also high.

The greifing factor is very large. Compare this to a market for leveraged financial derivatives. In a market with leveraged derivatives, you can make a claim with $10 that will pay out $100 if you are right. With normal market you don't have to have so much money locked up in contracts in order to sell your information. This means that normal markets for financial derivatives are a more affordable tool for selling information, in comparison to Numerai.


6) calculating whether it is more profitable to sell information in a normal market, or in Numerai

There are natural trade-offs between how profitable the contract can be, and how specific the information you provide is.

Financial derivatives make it easy to express relative confidence in claims by the relative amount of money you put at stake in different markets.
Financial derivatives make it easy to express relative confidence in value-space, by using leverage.

When you make a claim in Numerai, you lose all this specificity. You can't express relative confidence in different claims, you can't express relative confidence in different parts of value-space of a single claim. The loss of specificity means you are selling less information and can make less profit.

Lets use some numbers in an example. Bob knows that his company is about to release a monthly report that will cause the stock price to drop by about 5%. He knows that the price on the 2nd next month is normally distributed with a mean of 95% the previous value and a standard deviation of 1%.

If Bob uses a short with 40x leverage and 5% margin, then he would have a 50% chance to more than double his money, and 99.99994% odds of ending with more money than he started, and <0.00006% chance of losing all the money.
If Bob uses a short with 400x leverage and 0.5% margin, then he would have a 50% chance to 20x his money, and 99.99994% ods of ending with more money than he started, and <0.00006% chance of losing all the money.
The real limit to Bob's profit is in finding people to take the other side of his bets.

If Bob uses Numerai, then it isn't clear how much profit precisely he would make from this single prediction. This prediction could make his average prediction accuracy more accurate, and that might attract more customers.
We can calculate the cost to Bob in terms of griefing risk.
The griefing ratio isn't so important for this calculation, what matters more is the amount of money Bob puts into the griefing safety deposit. This is the total amount he can lose do to griefing, lets call it G.
P = how much profit Bob can make by tricking his customers into believing a lie.
In order to get people to trust his feed, Bob needs G > P.
The cost of running the price feed is G * ((odds that someone will grief attack Bob) + (interest rate of money) * (amount of blocks G is locked up for))

With leveraged derivatives Bob could multiply his money 50x by revealing a single secret.
Because of how much money needs to be locked in the griefing safety deposit, with Numerai Bob can only multiply his money <2x from a single secret revealed.

Bob needs to earn more profit than the cost of operations, otherwise he is losing money by trying to sell his information. It is unlikely that it would ever be profitable to sell information on Numerai, given the risks.

Possibly the only profitable way to sell info with Numerai is if you are planning to do a retirement attack.

7) Numerai cannot be used if you only have 1 secret to sell.
If I have extremely valuable information just once, and it wont recur again, then there is no way to use Numerai to profit.
I can't build up any reputation if I only have 1 piece of information to sell.
No one will sign up for my Numerai info feed if I don't have a history of good predictions.

I could sell 1 piece of info in a normal leveraged derivatives market. Markets don't depend on reputation the way Numerai does.

8) Parasite contracts.
Information wants to be free. Once you have one customer buying information from your feed, there is nothing stopping him from re-selling that information to everyone else.
The Nash equilibrium will be that the value of the information tends to zero. Everyone will be trying to undercut each other.
So in the Nash equilibrium situation, it is not profitable to sell on Numerai.

9) Info feeds don't aggregate data the way that markets do.
With feeds we can't know the relative confidence of the different information, so there is no way to combine the information to find out more accurate truths.
By aggregating all the info available, market distill the truth more accurately than anyone else.
No one will want to pay for innaccurate information from Numerai when you can freely get more accurate information by looking at the price of a market for derivatives.

10) People who want to buy information don't know how big the griefing security deposit should be.
Numerai uses a griefing security deposit from the users who want to sell information. This way we can punish them if they sell bad info.
In Numerai it isn't possible to know how big a particular person's security deposit would need to be for the information to be considered secure.
For rich users selling info, the deposit would need to be much higher, because rich users can make more profit from tricking us into believing lies.



Summary:
The information you can buy from Numerai is less accurate than what you could find out for free by looking at the price of a market for derivatives.
If you have information to sell, you could get a better price by selling it using leveraged derivatives instead of Numerai.


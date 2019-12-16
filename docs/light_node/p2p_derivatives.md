P2P derivatives
========

This tool is for making smart contracts.

[Here you can see a place where you can publish offers to trade p2p derivatives.](http://159.89.87.58:8090/main.html)

You can make a offer for a smart contract, and post it to a forum.
Anyone else can accept your offer and participate on the opposite side of the contract.

Examples
========

If there is a football game, Alice can post an offer to bet $100 on team A. Then anyone can take that offer, and make a tx that locks the $100 from Alice with $100 of their own money so that they are betting with Alice on the outcome of the football game in a new channel.

If Bob thinks that the price of VEO will go up, he can sell stablecoins on Amoveo, and this will give him long-veo. An asset that has higher leverage, so Bob can have even more exposure to Veo price risk.
If the price of Veo increases 10%, Bob's contract increases in value 20%.
If the price of Veo decreases 20%, Bob's contract decreases in value 40%.

Browser Light Node
========

The p2p derivatives tool is a feature supported by Amoveo's browser light node

You can download the light node onto your own computer from [here](https://github.com/zack-bitcoin/light-node-amoveo). This is the most secure way to run the light node:

I am serving this light node from my server, you can see it [here](http://159.89.87.58:8080/home.html) This is the home page that links to all the other pages in the light node.


Smart contract timeline of all steps
========

1) We need some question to bet on. We can use the [new oracle page](http://159.89.87.58:8080/new_oracle.html) to select new topics to bet on. Binary oracles are true/false questions. Scalar oracles can measure a price, or some other value in a range. [documentation for making good oracles](../basics/using_oracle.md)

Once you have created the oracle, you will receive the oracle ID. You use this so the smart contract knows which oracle will determine who gets the money.

2) Someone creates an offer for a new smart contract.
[this page](http://159.89.87.58:8080/otc_derivatives.html) can be used for making new contract offers. Once you have a contract offer, you can publish it in [Amoveo discord #trading channel](https://discord.gg/xJQcVaT) or anywhere you think someone who wants to bet against you will see it.

3) Someone accepts the offer of a smart contract.
[this page](http://159.89.87.58:8080/otc_listener.html) can be used to accept a smart contract offer.

4) the event occurs. Now we know who won the football game or election, or whatever we are betting on.

5) we close the smart contract to get our money out.
[this page](http://159.89.87.58:8080/otc_finisher.html) can be used to close the channel.
If both participants work together, closing the channel takes only 1 block.
If one participant refuses to cooperate, then you have to wait for the oracle to settle, and return to the otc_finisher.html page a couple times to completely get your money out.

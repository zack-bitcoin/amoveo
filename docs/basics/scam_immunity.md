Scam Immunity
========

The goal of this document is to explain very basic principles from finance, game theory, economics, and cryptography. Very basic principles that have been understood and agreed upon for decades.
The goal is that these principles should be simple enough for anyone to quickly understand.
My hope is that by learning these principles, you can protect yourself from the majority of scams in the cryptocurrency space.

Finance
========

Finance, also known as "risk management", is a system of tools for minimizing the odds of losing money. Finance is a zero-sum game. For one person to win, someone else needs to lose.

So, if anyone tries to sell you a financial tool that generates value or earns profit, it is a scam.

The most basic financial tool is called a "derivative". That is where 2 people lock money into a contract, and after a period of time the money in that contract is distributed back to the 2 participants, and the amount of money that each receives is based on some public information, like the price of Tesla shares, for example. This allows one of the participants to hold extra Tesla risk, and the other to hold less Tesla risk.

The only secure way to participate in derivatives contracts is using a market with single price batches. That means that everyone who makes a trade during a certain period of time, a 5 minute period of time for example, they are all trading at the same price. This was famously discovered by John Nash when he designed the second price auction.

If anyone claims to have secure stablecoins, but there is no market with single price batches, then you can know that it is a scam and that their stablecoin does not work.

Economics
======

The basic principle from economics that is important to be aware of is called "supply and demand".
Also known as "if it seems too good to be true, then it probably is."

When it comes to investing, the supply and demand is expressed as a natural balance between risk and reward.
The risk/reward ratio of each investment is always equal to, or worse than, the same level as the current interest rate of the economy.

If there is a good opportunity to profit from investment, then many investors will want to be involved. More investors will keep showing up, so the demand for further investment will keep going down, so the reward paid to investors will keep going down, until the reward/risk ratio is about the same as the current interest rate of the economy.

If a particular investment has the potential to pay out a big reward, it is necessarily the case that this investment also has high risks of losing a lot of money.

If someone claims that they can pay out big rewards, and that the risks are low, then they are a scammer.

There is an important exception. If you are more knowledgable than the average investor participating in a market, then you could potentially profit from your insider knowledge and earn much more than the standard interest rate.

Cryptography
========

For cryptocurrency there are 2 important cryptographic constructions that you need to know about. Hash functions, and public-key cryptography.

A hash function is a function that takes any data as the input, and gives out a fixed sized output. The hash function in bitcoin is called SHA256, it has an output of 256-bits, or 32 bytes.
Given the same input, a hash function will always produce the same output.
If you only know the output, it is impossible to find out the input.
It is impossible to find 2 different inputs that result in identical outputs.

An example use-case of a hash function. If I want to commit to a secret value, and reveal it later, and I want it to be impossible for me to edit the secret in between.
I could publish the hash of the secret message. Then eventually when I reveal the message, everyone can check that the hash of the message is the same as what I had published, so they can be sure that I didn't edit it in between.

Public key cryptography in the context of blockchains is used as a system for making secure signatures. Each person generates a random secret called the private key, and uses that secret to build a public key.
You can use your private key to sign any piece of data, and then anyone else can use your public key to verify that you did in fact sign that data.
It is impossible for anyone who doesn't know your random secret to sign for you.

An example use-case of public key cryptography. If you want to spend some money to someone, we want it to be the case that only you are able to spend the money, and no one else can spend it. So we would require you to cryptographically sign the message explaining that you want to spend the money.

All other cryptographic constructs used in blockchain are very controversial. It isn't clear if any other cryptography tools could be useful in blockchain, or if only hash functions and signatures are useful. 

Game Theory
=========

Blockchains involve repeated games between many participants. In order to reason about blockchains, it is necessary that the strategy that the players are using, it is stable. Either the players are using the same strategy every time, or there is some kind of negative feedback, so if players divert from the expected strategy, the feedback mechanism pushes everyone back to the expected strategy.

Even if we can know that a blockchain's game is stable, that does not prove that the blockchain is secure. Stability is necessary so that we can make any sort of reliable prediction about how the blockchain will behave in the future. If a blockchain was built on an instable game, then we could not make any sort of prediction about how it will work tomorrow. It would change in unpredictable ways. So we would be unable to know if it will continue being a secure blockchain in the future.

All secure blockchains are built on stable games. But not all stable games enable secure blockchains.

We have techniques to prove that a game is instable.
We do not have any tool to prove that a game is stable.

It is similar to how science works. Scientific experiments cannot prove any hypothesis is true. They can only prove a hypothesis false. The way we come to have confidence that a hypothesis is very likely to be true is if many different scientists work very hard to try and disprove it, and they all fail to dispove it. 

Similarly, the way we gain confidence that a particular game is stable is if many people try to prove that it is instable, and they all fail.

If anyone claims to have a "proof that their system is secure", they are a scammer.
If anyone claims to have a "proof that their blockchain's game has a stable equilibrium", they are a scammer.

The best a person could claim for their system is that "many people have tried and failed to find vulnerabilities".


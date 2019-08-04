Theory of trust
==========

Historically, trust was seen as a positive thing. The glue that held people together.
In the world of blockchain, trust is a bad thing. If you are trusting someone not to steal your money, then that means it is possible for them to steal your money, so it is not secure.
In this way, trustfulness is the opposite of security.

There is a spectrum of different kinds of trust that we need to identify.
We need to find some general principles for estimating how much each level of trust would cost to solve a particular application.
It would be great if we could find some general principles to quickly know the strongest level of security that can be achieved for a given application.


From strongest security to weakest, the spectrum of trust:

1) provably secure cryptographic protocols. An attacker willing to spend any amount of money can't meaningfully interupt the mechanism or take anyone's money.
* hash algorithms
* public private key cryptography
[learn more about security level 1 here](trust_theory_1.1.md)
2) mechanisms where any attacker willing to spend A of their own money can destroy B value of someone else's. where A > B.
[learn more about security level 2 here](trust_theory_2.2.md)
3) same as (2) but 0 =< A =< B.
* if a blockchain mechanism has this kind of level of trust, it is usually considered a bug that needs to be patched. These are griefing attacks https://consensys.github.io/smart-contract-best-practices/known_attacks/#insufficient-gas-griefing
4) same as (3), but A < 0.

for the trust level of a protocol, we record 2 numbers. The first number is how bad attacks can be where we can know the attack occured, and the second is how bad the attacks can be where the attack is indistinguishible from unlucky randomness.

So the first digit is always >= the second


so 1.1 is perfect security.

2.1 means there is a way to spend X of your own money to destroy Y of someone else's, but you will get caught.

4.1 means money can get stolen outright, but we will know who did it.

4.4 is the worst case. for example: intrade. They could front run all your trade to rob you, and you wouldn't even know until you have done enough trades to get statistically significant data.

If you combine 2 mechanisms with different levels of trust together, you take the bigger of the first digits, and the bigger of the second digits.
1.1 + 4.1 = 4.1;
2.2 + 3.1 = 3.2;
1.1 + 1.1 = 1.1;


In the context of blockchains, people start using words like "cryptoeconomically secure" or "trust free", but which levels of trust does that really mean?
In Amoveo, we try to keep every aspect of the blockchains and channels at level 2.1. We do not exceed trust level 2.1, to the best of my knowledge.

I think if the second digit should be called the "unsealed trust level" or "sincere trust", because the latin roots of "sincere" are "without" and "seal".
They are similar to an unsealed bottle, in that you can't know if someone has tampered with it.
The first digit can be the "sealed trust", or "insincere trust".


4.X are significantly worse than the rest in that the attacker can profit from attacking.
A mechanism of level 4 will inevitably be destroyed.

Every level of trust less secure than 2.2 suffer from the credible commitment to attack problem. That is where someone very rich makes a believable commitment to destroying the mechanism. If the community believes that it will get destroyed, then they will abandon the mechanism, so the rich person doesn't actually have to pay for the attack.
At level 1.1, it is not possible to make a credible commitment to destroy the mechanism, because the mechanism cannot be destroyed.
At level 2.2, it is possible to credibly commit to destroying the mechanism, but the existing participants do not abandon the mechanism, because they can make enough profit during the attack that they would prefer for the attack to happen.


The cost of using a mechanism increases with the level of trust.
* At level 1.1, verifying a signature over a binary is a purely cryptographic algorithm. It costs nothing to do this, it is free.
* At level 2.1/2.2 there are miners or some other parties who we need to pay fees to in order to use the protocol. The miners can't rob us, so the payment is just to give you a good spot in line. It isn't for security.
* At level 3.1/3.2 we are still paying for a spot in line, but we are also paying for some security. There could be some occational retirement attacks that happen because competitor blockchain is bribing people on our blockchain to make it happen. The higher of a fee you pay, the less frequently these attacks will occur.
* Level 3.3 is similar to 3.2, except without any reputation system. So paying a higher fee wont make you any safer.
* at level 4.1/4.2, you are deposting your money into a centrally controlled service, and hoping that the fees you pay are enough to convince them not to steal any of your money.
* At level 4.4 your money can get stolen, and you don't know who did it. You can't tell if the system is working correctly and you are unlucky, or if you got robbed. This is a 100% trustful system.


More trust = less security. More trust = higher fees.
So as mechanism designers, it is always preferable for us to create mechanisms that have the lowest level of trust possible.

This is why we should create a general theory of trust, so that we can quickly calculate the lowest level of trust needed for a given application.

Not only is this useful for mechanism designers, it is useful for investors as well.
If an investor can calculate that a mechanism is not using the lowest level of trust possible, then the investor can know that this project will be more expensive and less secure than the competition.



Applying these tools to existing projects
=======

Bitcoin hivemind and Augur are 3.2
The oracle participants can lie to cheat in a market, but they lose more money than they earn from the markets.




Examples
======

1.1
=====

public key cryptography. hash algorithms. merkel proofs.

2.1
=====

The amoveo markets are 2.1 level secure. It is a centralized market, so if the market maker cheats by publishing multiple different prices during the same batch, then we know he is the only one who could have done it. And it is not profitable for him, because in that case he would lose all the money in all the smart contracts.

2.2
=====

Nakamoto consensus is 2.2.
An attacker could build tons of ASICS and take over the network, but it would be more expensive than the value they could destroy.
The attacker building ASICS could be anonymous, so it is 2.2, not 2.1

3.1
=====

Bitrated is 3.1, as long as the fee is high enough.
The escrow agent could accept a bribe and help to steal the money, but they are the only one who can do this. So it is not anonymous.
2 of 3 wallets used for escrow are 3.1 level trust, as long as the escrow agent is being paid a high enough fee.

The fee needs to be high enough so that the long term expected profit of being an escrow agent exceeds the short term potential profit of stealing some money.

3.2
=====

Under the assumptions that
1) parasite contracts wont happen
2) P+epsilon attacks wont happen

then Augur/Bitcoin hivemind are 3.2
There is a team of people holding votecoins who can potentially make the oracle lie on outcomes to steal money, but we know which accounts are holding those votecoins, so we can potentially punish them or stop trusting them.
The 0.2 is because anyone can anonymously just buy up half the votecoins to make it lie, but that is supposed to be more expensive than what they can steal.

3.3
=====

Email. Anyone can send you messages anonymously, and it costs more to deal with spam than it does to send it.


4.1
======

Bitrated where the fee for the escrow agent is too low. If the expected profit of robbing this one trade is bigger than the expected profit from maintaining the escrow account's reputation, then it only has level 4 security. But, since only that one agent could have done the theft, it is not anonymous.

4.2
======

If you consider bitrated the service in combination with the bitcoin blockchain it is built on top of, the bitcoin blockchain can be attacked anonymously at level 2, and if the fee is low, then the bitrated service can be attacked with 4.1, so in combination, your security is 4.2

4.3
=======

If you put your money onto trusted centralized server to gamble in cryptographically enforced games of chance. Then that means the server can steal your money, so it is level 4 security with the server, but if they just directly robbed you, then you would know it happened and that the server did it.
And in addition, lets imagine there is some bug that lets the other users spend $1 to cause you to lose $2 to the server. So any anonymous user can attack you at level 3.
This situation would be 4.3 level trust.

4.4
======

If you put your money into a trusted centralized server to gamble in the price of derivatives, and the order of trades is not strictly enforced, so that other users or the server are able to front-run your trades, then this is 4.4 level secure.
You don't know who is robbing you, or if it is even happening. Maybe you are just an unlucky trader.




Security level
=======

1,2,3,4 are the discrete trust levels. They increase with increasing amount of trust.
The security level of a mechanism is any number between -1 and infinity. It increases with increasing amounts of security.

trust level 1 = security level infinity.

trust level 2 = security level in the range between 1 and infinity.

trust level 3 = security level in the range between 0 and 1.

trust level 4 = security level in the range between -1 and 0.

you calculate your security level like this:
If the attacker's change in balance due to the attack is A, and the defender's loss is B, then the security level is A/B .

security levels are a more precise way of communicating the security of a blockchain in comparison to trust levels, but it is harder to calculate, so maybe it isn't worth using.
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
2) mechanisms where any attacker willing to spend A of their own money can destroy B value of someone else's. where A > B.
3) same as (2) but 0 =< A =< B.
* if a blockchain mechanism has this kind of level of trust, it is usually considered a bug that needs to be patched. These are griefing attacks https://consensys.github.io/smart-contract-best-practices/known_attacks/#insufficient-gas-griefing
4) same as (3), but A < 0.

for the trust level of a protocol, we record 2 numbers. The first number is how bad attacks can be where we can know the attack occured, and the second is how bad the attacks can be where the attack is indistinguishible from unlucky randomness.
So the first digit is always >= the second

so 1.1 is perfect security.
2.1 means there is a to spend X of your own money to destroy Y of someone else's, but you will get caught.

4.1 means money can get stolen outright, but we will know who did it.
4.4 is the worst case. for example: intrade. They could front run all your trade to rob you, and you wouldn't even know until you have done enough trades to get statistically significant data.

If you combine 2 mechanisms with different levels of trust together, you take the bigger of the first digits, and the bigger of the second digits.
1.1 + 4.1 = 4.1
2.2 + 3.1 = 3.2
1.1 + 1.1 = 1.1


In the context of blockchains, people start using words like "cryptoeconomically secure" or "trust free", but which levels of trust does that really mean?
In Amoveo, we try to keep every aspect of the blockchains and channels at level 1.1. We do not exceed trust level 1.1, to the best of my knowledge.

I think if the second digit should be called the "unsealed trust level" or "sincere trust level", because the latin roots of "sincere" are "without" and "seal".
They are similar to an unsealed bottle, in that you can't know if someone has tampered with it.
The first digit can be the "sealed trust"


4.X are significantly worse than the rest in that the attacker can profit from attacking.
A mechanism of level 4 will inevitably be destroyed.

Every level of trust besides 1.1 suffer from the credible commitment to attack problem. That is where someone very rich makes a believable commitment to destroying the mechanism. If the community believes that it will get destroyed, then they will abandon the mechanism, so the rich person doesn't actually have to pay for the attack.
If you don't have any rich enemy, then that might not be an issue for your protocol, so level 2.2 might be a reasonable level of trust.
There are many mechanisms out there of level 2.2 that are considered secure.


The cost of using a mechanism often increases with the level of trust.
at level 1.1, verifying a signature over a binary is a purely cryptographic algorithm. It costs nothing to do this, it is free.
at level 4.4, you are deposting your money into a centrally controlled service, and hoping that the fees you pay are enough to convince them not to steal any of your money.


More trust = less security. More trust = higher fees.
So as mechanism designers, it is always preferable for us to create mechanisms that have the lowest level of trust possible.

This is why we should create a general theory of trust, so that we can quickly calculate the lowest level of trust needed for a given application.

Not only is this useful for mechanism designers, it is useful for investors as well.
If an investor can calculate that a mechanism is not using the lowest level of trust possible, then the investor can know that this project will be more expensive and less secure than the competition.


in total, there are 10 levels of trust identified here. If you can make a general proof that mechanism with trust 3.2 can always be improved to be 2.2, that would be a great discovery.

If you can prove that any mechanism can be written in a 1.1 form, or at least give a broad range of mechanisms that can be in 1.1 form, that would be great.

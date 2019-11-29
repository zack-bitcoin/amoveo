Blockchain Privacy
==========

[In this paper we made a big connection to show that higher security = lower trust = lower fees](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md)

It is known that if we increase scalability, fees will go down proportionally.

So we already have a unified theory connecting scalability, fee size, trust, and security.

The goal of this paper is to unify our model of privacy with all these other things, so that we can understand blockchains better.

What is privacy?
==========

Blockchain privacy means the ability to own value, transfer value, and participate in smart contracts, and we want some or all aspects of your interaction to be impossible for anyone to find out.

A good model of privacy would give us a formula to calculate the cost in terms of computation, money, and time, in order to achieve a particular level of linkability between value you received and value you spent.


Blending in
=======

Paul Sztorc wrote a nice essay on this topic http://www.truthcoin.info/blog/expensive-privacy/

Paul talks about how the majority of users don't care about privacy, and they just use the cheapest solution for payments and smart contracts.

He explains that in order for privacy to be useful, we need to be blending in not just with other people willing to pay extra for privacy. We need to blend in with the users who don't care about privacy.

Grin's solution for making quantities private works, because it results in transactions that are smaller, and a smaller database to remember balances.


Quantities
========

Making the quantity of money sent in a tx private is a solved problem. The project called "Grin" found a way to use pedersen commitments to achieve this.

Making the quantities private is not necessary, but it can be a useful part of preventing linkability.

Linkability
=======

linkability is a measure of how much confidence we have about the path that some money took between different accounts.

Anonymity sets
========

An anonymity set is when 2 or more blockchain transactions are mixed together such that we can't tell which inputs are associated to which outputs.
If N transactions are perfectly mixed in an anonymity set, then that means the linkability between inputs and outputs is 1/N.


Network level linkability
========

For example, if someone knew or could prove that your IP address or human identity is connected to some other ip address or human identity in particular.

In the case of bitcoin, the recipient has privacy by default.

An attacker who controls many full nodes may be able to identify the first full node that knew about your tx. But if you used VPN or TOR or something like that to disguise your identification against that first full node, then you can be private.

So network level linkability is solved even in the case of bitcoin.

Blockchain level linkability
========

This is when someone can know that there is a connection between two identifiable parts of the on-chain money.

For example, if you spend some BTC from one address to another, the path is written on the bitcoin blockchain. Everyone can verify that the money went from one address to another, they have 100% certainty of the path the money followed.
So linkability in this case is 100% or 1.

Another example. If I have a bitcoin private key on a piece of paper, and I don't have any backup. I could hand that paper to someone who I want to pay the bitcoin to.
In this case, no information is written on the bitcoin blockchain.
No one can even know that I have spent my bitcoin.
So linkability in this case is 0% or 0.

Zcash is a project that solves blockchain level linkability, but it costs more than alternative designs. So according to Paul Sztorc's reasoning, it does not work to add security.

One way to reduce blockchain level linkability is by spending your money to yourself many times. This is the strategy recommended by Paul Sztorc's paper.


Connecting scalability to privacy
============

If spending your money to yourself really is the best way to decrease blockchain-level-linkability, then that means the cost of a certain level of privacy is proportional to the cost of spending your money to yourself.
So a blockchain that can handle twice as many txs per second at a given level of fees, it would also be twice as cheap to achieve a given level of privacy.

This shows that whatever blockchain solution is the most scalable, it is also the most private.

[I compare many scaling strategies here](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/sharding.md)

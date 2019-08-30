Bitcoin Review
=======

Bitcoin is currently the most successful blockchain that has existed.
Given its success, why would we bother studying bitcoin's security model?

Motivation for Bitcoin Maximalists
=========

Like all software, bitcoin does require maintenance and updates to stay compatible with the rest of the internet. Given the fact that we will need to make some sort of changes, we would like to have some model of why bitcoin is secure. That way, we can check if our change will break the security model.
This can prevent bitcoin from breaking so often when we update.

Motivations for altcoiners
=========

We all like to draw parallels between our projects and bitcoin, to try and show why our projects are also secure. But are these comparisons really valid?
To be sure, we would need a model of bitcoin's security.

Motivations for investors
=========

If we know why bitcoin is secure, this can help us to know which blockchain projects do or do not have a chance to compete with bitcoin. To help you avoid bad investments.
If we manage to show that bitcoin is not secure, this would be critical information for investors to learn as soon as possible. So we can start figuring out whether bitcoin will change to be secure, or if some other blockchain will replace it.
If it turns out that blockchain cannot work, it is better to learn this sooner instead of later when all the value in all the blockchains is gone.



What would a model of Bitcoin's security look like?
==========

It would be a tool so that given any example of a strategy to attack bitcoin, we can use this tool to calculate how the attack will fail.

1) It needs to solve the Byzantine generals problems. Which basically means that we can all eventually agree on the new consensus state. So there is one version of history that we agree has happened.

2) It needs to solve the double-spend problem. Which means it needs to have finality. If you wait enough time, then the history that leads up to the current consensus state is permanent.

3) It needs to solve the soft-fork-bribe problem. There is a proof that any modification to a blockchains consensus rules, it can be done entirely by making complicated censorship rules. So that means that an attacker who is able to censor arbitrary txs, they can change anything about the blockchain and who has what money.

There is an idea from game theory called "tragedy of the commons" or "market failure". It basically means that you can pay a large number of people a small amount of money as bribes, and they will make decisions that results in the destruction of something worth much more than the sum of the bribes.

So the attack we are worried about, is that the attacker will use small bribes to cause users or miners to make decisions that results in certain txs being censored, causing a soft fork update that lets the attacker steal lots of bitcoin.

A solution to (3) cannot involve undoing much history. If we rewrite history, then we would be making ourselves vulnerable to (2).


Blockchain BFT model
==========

This kind of model is a favorite for people with a computer science or databases background.

People working on proof-of-stake prefer this model, because it is possible to make a PoS blockchain that enforces this security model. For example, the Cosmos and Ethereum communities use this model to describe blockchain security.

BFT model basically comes down to finding a way to enforce these 2 rules:

- safety: bad things do not happen
- liveliness: good things eventually do happen

The BFT model is based on database research that is only applicable for showing that the nodes all agree on the current state.

We can extend the BFT model to solve (2) using the fact that re-writing history is expensive.

explains: 1, 2
does not explain: 3

Value Maximization model
=========

This kind of model is a favorite of people with an economics, statistics, and monetary policy theory backgrounds.
This is the model that a lot of Bitcoin Maximalists use.

In this model, we say that an optimal version of bitcoin is one that always makes decisions that results in a higher price per BTC, and we try to prove that the Bitcoin we use today is a good enough approximation of this optimal version.

So in this model, proving that bitcoin is secure is the same as proving that the version of history that we agree on is the version of history that results in the highest price per BTC.

This model has a built-in vulnerability: if an attacker can credibly commit to destroying N value conditional on whether we pay them a bribe, then that means our model would require us to be willing to pay N-1 value as a bribe to prevent the attacker from destroying N.

You might think that the model can be saved by making some finite list of exceptions to deal with credible commits to destroy, but there is a fundamental problem with this model that cannot be solved with any number of exceptions.
This model is reliant on circular logic.

A fact outside our control is this: If we have consensus that one version of history is valid, then that must be the most valuable version of history. Every other version is worthless, because no one will accept payments from it, because we have consensus to not receive payments from it.

So, if we attempt to define our consensus mechanism as a mechanism that follows the most valuable choice, this is a circular definition.

Any attacker who can credibly commit to forcing one version of history to be more valuable, the rest of the network would follow along and make that version of history the official version.

Having a circular definition breaks the rules of logic, it makes it so we can't use reason to understand this model. It is an unreasonable model for bitcoin's security.
Since it is unreasonable, you can prove contradictions in this model.
You can prove that PoS works, you can prove that PoS does not work, you can prove that 1=2.


Futarchy model
=========

The futarchy model is similar to the value maximization model, with one key difference.
Instead of asking which version of history is more valuable now, we are conditionally asking which version of history would be more valuable if we could all agree that it is the valid version of history?

The value maximization model might ignore a version of history because it is worthless.
But the futarchy model would accept it, because it doesn't matter that it is worthless now. What matters is how valuable it will be after we have accepted it.

The futarchy model solves the circular reasoning from the value maximization model.

But, the futarchy model suffers from a big draw-back. We can only build futarchy contracts if we already have an existing consensus mechanism to enforce them.

The futarchy model assumes that it is possible for us to enforce futarchy contracts in order to make the measurements necessary to know which version of history would be more valuable.
But, we can only enforce a futarchy contract if we already have a consensus mechanism to allow us to agree on the state of the futarchy contracts.

The futarchy model is a way to extend an existing consensus mechanism to achieve consensus over more data, but you can't use it to bootstrap the initial consensus mechanism.

The futarchy model of blockchain security is a popular model for updating the rules in existing blockchains http://www.truthcoin.info/blog/fork-futures/

This security model is popular with people researching blockchain oracles, since an oracle is an extention of a blockchain's consensus over more data.

This security model is a common tool for blockchain scalability strategies. It is a way to extend one blockchain's consensus to enforce rules on side-chains or shards. It is a way to achieve security guarantees over data that you have not personally downloaded.
Usually they use the term "fraud proofs" to describe strategies that use this security model to achieve scalability goals.
http://www.truthcoin.info/blog/fraud-proofs/


no-chain model
=========

In order to launch new blockchains, and to do hard forks, there must exist some consensus mechanism external to any blockchain that allows us to come to agreement on some facts.

This is the model for the resolution to the famous DAO hack in Ethereum. Since the money was locked up for a long enough period of time, the network had time to consider the situation, and agree on what a hard update to fix it would look like.

This consensus mechanism is limited in space. We can only consider so many questions simultaniously.
This consensus mechanism is limited in time. We need a lot of time to fully consider a question before we can answer it.

This is the model for Amoveo's oracle. Since the money in the oracle is locked up for a long enough period of time, we can do a hard update to force the oracle to resolve in the honest direction.


Crabs in a bucket model
=========

This kind of model is more favorable to people with a math or biology background.

It is not possible to layer a stable consensus mechanism on top of a stable consensus mechanism.
One layer can only be as stable as the previous layer is unstable.

I will show this using proof by contradiction.
Imagine there are 2 different stable consensus mechanisms. A is the first, and B is built on top.
Imagine A and B want to make a contradictory decision.
Since A is stable, A will succeed. Since B is stable, B will succeed.
But they can't both succeed, since the decisions are contradictory.
If B has control over some part of the state, then A necessarily cannot control that part of the state.
If A has control over some part, then B necessarily does not have control over that part.
So this shows that it is not possible to have 2 different stable consensus mechanisms layered on top of each other.

The consensus mechanism can only provide security guarantees over part of the consensus state if the miners/validators are unable to cooperate to control that part of the consensus state.

In this model, proving that bitcoin is secure is the same as proving that miners cannot trust each other enough to cooperate. Their relationships needs to be adversarial. Even if the miners have access to other smart contract platforms where they can make arbitrary contracts together, it needs to still be impossible for the miners to cooperate.

explains: 1, 2, 3

The crabs in a bucket model breaks down if ever there is one individual who has physical control over the majority of hashpower. Since an individual is always able to coordinate with himself, there is no way to prevent him from taking control of the consensus. An attacker with physical control of the majority of hashpower could steal any BTC, and change any consensus rules that define bitcoin.

Crabs in a bucket model is very un-popular with PoS blockchain communities, since we can use this model to show that PoS can not work https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

Forking Concerns
=======
March 2019

If the outcome of an oracle is ambiguous, and a lot of money will be determined by the outcome, can it cause the blockchain to fork?

This question is important because if Amoveo started rapidly forking into thousands of chains, it would destroy the majority of utility of Amoveo.

Keep in mind that we will have a week to argue and plan our fork if we know that an oracle will cause a split.

## Bitcoin as an example

In bitcoin, every time 2 blocks are found at the same height, this is an ambiguous decision worth at least the block reward, and bitcoin heals from this quickly.
This shows that low value ambiguous forks of equal height can be healed easily by Nakamoto consensus.

In bitcoin, any time someone spends a large enough value of bitcoins, this creates an incentive for them to rebuild bitcoin from before when they made the payment. They do not succeed in causing double-spends.
This shows that high value ambigious forks can be prevented, as long as there are enough confirmations. 

## 5 types of teams

For a fork to survive, then all 5 of these groups need to have participants on both sides of the fork:
* investors
* miners
* mining pools
* developers
* users (<--- most powerful limits are from here.)

That means that any one of these layers can prevent an attack-fork from succeeding.
We can look at each of these groups to find boundaries for when these attacks cannot occur.

## Investors

If a fork occurs, everyone holding veo is incentivized to keep their money on both side, just in case. So we can't depend on investors to resolve the fork.

## Miners

If users are paying something for veo on both sides, then miners are incentivized to mine for both, so we can't depend on miners to resolve the fork, unless they have ASICS. If they invested in ASICS for Amoveo, then they are interested in the long term success of Amoveo and will be interested in only participating in one side to resolve the fork.

But, if the difficulty keeps falling on the other side, and if the price stays high enough, we can't depend on the miners not to jump ship. This is because the individual cost of switching is low, so even a small benefit is enough to cause them to switch.
The larger the portion of your mining power of the total, the less your incentive to join the attacker's side, because you have more hardware that can lose value.
So we get more secure with ASICS, and more secure if there are fewer big mining pools instead of many small ones.

Once we have ASICS we might be secure against these kinds of attack forks entirely. Whichever side has more ASICS, they can commit double-spends and censorship against the small side to destroy it.

## Mining Pools

The cost of launching a new mining pool is low, if the attacker has enough financial interest in the outcome, then launching a new mining pool could be feasible for them.

## Developers

The developers have a large incentive to prevent the fork, because if Amoveo loses value, then they will get paid less for the software they write.
Developers have a better chance of organizing together to prevent a fork, but with such a small number of people the randomness of psychology starts to matter. If someone feels unappreciated or wants to try something experimental, they might join a fork just to have more independence and have the freedom to try out something different.
If a "fork attack" involves trying out something new and experimental, it is hard to call that an "attack". They are doing valuable experimentation.

## Users

If Amoveo is to fork and both sides are to survive, then that means both sides have active users.
Users obey economic incentives when deciding which blockchain to use for their contracts.

If the veo on one side of a fork is worth less, it will have more volatility, and be a less suitable platform for financial derivatives, unless the txs fees are so much lower as to overcome this difference.
Assuming that volatility and fees scale together, and that the attacker is willing to make fees lower to help their attack succeed,
As long as the majority cost of a tx is coming from volatility instead of fees, we are secure against a fork surviving.
A 1 month veo contract locking up $100 currently costs around $0.50 in fees currently. So as long as the loss due to volatility is >0.5% per month, and the median $1 locked in contracts is locked in a contract of size $100 or bigger, then we are secure against these kinds of forks.
Currently, volatility is very high. the price is jumping around by 30%+ in the same day.
So we are currently secure against this kind of fork attack.

The difficulty in forking users has a sort of economical feedback. The network keeps splitting, until each fork reaches about the same size, where fees and the cost due to volatility are about the same.
Since they are all competing, the cost due to fees would be pressed down to the actual cost of providing the service. So each of the competing networks would be very large to have such small volatility.
This sort of economic feedback means that each competing fork will be such a large part of the blockchain economy, that it is not an issue to fork. Each one will stay large and important enough that Amoveo wont die in thousands of rapid forks.


## Summary

Developers, Users, and Miners provide the most important limits for these fork attacks.
Developer are less secure when the team of developers is socially splitting, or they have different visions for the future of Amoveo
Users are less secure if the tx fees get too high.
Miners are less secure if there are many small miners instead of few big ones.

As the network grows, Developers and Users become easier to fork, but miners become harder to fork.


For an oracle to cause a fork, all these things need to happen at the same time:
1) the result of the oracle question needs to be ambiguous between true/false/bad
2) a lot of money needs to be at risk for this oracle result, that way someone is incentivized to cause the fork.
3) Either we don't have ASICS, or the ASIC miners are choosing not to attack the smaller side to prevent the fork.
4) The community of developers needs to have already forked.
5) The cost of using the blockchain due to fees must be greater than the cost of using the blockchain due to volatility. (which usually only happens if Amoveo is very very big.)

It is hard to get (2) to happen if (1) is already happening. This makes causing a fork very difficult, and it gets more difficult over time as we learn the tricks for causing forks. If there is concern of fork, people will avoid risking money on the oracle, and we will have an easier time agreeing that it is a "bad question" oracle.

Overall, it is so rare that all 5 of these things happen at the same time, so it is not possible that Amovoe will die in thousands of forks.
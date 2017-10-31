# November 2017 Blockchain Oracle Progress Report.

### state of the competition between the teams.

This month we will focus on the oracle religions.
Where knowledge ends, we depend upon hunches and beliefs.
The oracle teams have settled into their own camps, with different opinions about how oracles will work.
So this month, each oracle religion and market religion will be compared.
Each project has a market religion, and an oracle religion.

Market religions: off-chain markets with single-price batches, off-chain brokers, on-chain markets.

Oracle religions: subcurrency voting oracles, betting oracles, public feeds.

Teams: [Gnosis](https://gnosis.pm/), [Augur](https://augur.net/), [Bitshares](https://bitshares.org/), [Bitcoin Hivemind](http://bitcoinhivemind.com/), [Zen](https://www.zenprotocol.com/), [Aeternity](https://www.aeternity.com/), [Amoveo](https://github.com/zack-bitcoin/amoveo)



| teams\religions | market religion | oracle religion |
| --- | --- | --- |
| Amoveo | off-chain markets with single-price batches | betting oracles |
| Group Gnosis | off-chain brokers + on-chain markets | subcurrency voting + betting + public feeds |
| Augur | off-chain brokers + on-chain markets | subcurrency voting oracles |
| Bitcoin Hivemind | off-chain brokers + on-chain markets | subcurrency voting oracles |
| Aeternity | none | none |
| Bitshares | on-chain markets | public feeds |
| Zen | off-chain brokers | public feeds |

# first looking at the market religions

## on-chain markets
 This type of market does not scale. Big markets are better, so this is almost certainly a bad design. That is why most projects that use this type of market also allow for some type of off-chain trading as well.
 This type of market depends upon on-chain order books, and an on-chain market-maker or liquidity maker.

Gnosis, Augur, Bitshares, and Bitcoin Hivemind are all following this religion.


## off-chain brokers
 This type of market is in channels, so it is scalable.
 This type of market only depends upon simple channels, so it is easy to implement.
 This type of market might be better if first mover advantage is important.
 This type of market depends upon channels that can settle into the same bet-tokens that are produced by on-chain markets.
 These type of markets depend upon channels with hashlocking.

Group Gnosis, Augur, Bitcoin Hivemind, and Zen are all following this religion.


## Off-chain markets with single-price batches.
 This type of market is in channels, so it is scalable.
 This type of market will be more affordable than off-chain brokers, because you don't have to pay the brokers.
 This type of market will be more affordable than on-chain markets, because you don't have to pay tx fees.
 This type of market is more complicated.
 This type of market will win if having a better price than the competitiors is important.
 This type of market depends upon turing complete state channels.

Amoveo follows this religion.


# next looking at the oracle religions

## public feeds

This type of oracle is probably insecure.
People make bets that reference some publicly posted information.
They reference public information before it is posted.
This is insecure, because the person you are betting with could convince the person who publishes the public information to publish a lie instead.

Group Gnosis, Bitshares, and Zen follow this religion.


## subcurrency voting oracles

This type of oracle is expensive, because the users of the oracle have to pay the subcurrency owners money. The subcurrency owners need to think that the long term benefit of loyalty exceeds the short term benefit of stealing all the money from the market. So there needs to be fees to pay the oracle owners. Additionally, the oracle can only be secure if the amount of money being bet in the market is less than the total marketcap of the voting subcurrency. So it can only support small markets.

Group Gnosis, Augur, and Bitcoin Hivemind follow this religion.


## betting oracles
This type of oracle is very simple, and leans heavily upon the blockchain's consensus mechanism for support. It is untested. Leaning on the consensus mechanism this way may make the consensus mechanism insecure.
If it works, then it should be able to support markets with any volume of betting.
If it works, then it should be much more affordable than the alternatives.

Group Gnosis, and Amoveo follow this religion.


[Discuss this on reddit](https://www.reddit.com/r/Amoveo/comments/79kzm4/comparing_strategies_of_7_teams_competing_for_the/)
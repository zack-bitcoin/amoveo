Why Bonding Curves are a bad design choice


Here is a recent article by someone who is not me, describing bonding curves in Ethreum.
https://medium.com/coinmonks/bonding-curve-a-potential-solution-for-tokenizing-flash-organization-fb9a31fb37b5

I first heard of bonding curves at a presentation by the Bancor team, and I had conversations with the team members afterwards.

Bonding curves are a great example for learning about cryptoeconomics. In this post I will use game theory to compare bonding curves with better alternatives.



glossary:
* subcurrency - any currency on a blockchain other than it's native currency. in particular, I call the new currency produced by a bonded curve a "subcurrency".
* native currency - in the case of Ethereum, the native currency is Eth. This is the currency that block rewards are paid in.


Definition of a bonding curve:
A smart contract on a blockchain like ethereum. It holds lots of native currency as backing for a subcurrency it produces. You can trade the contract native currency for subcurrency at any time. The exchange rate that the contract trades at changes, the larger amount of subcurrency in existence, the higher the price of the subcurrency.


The main point I will try to be arguing for in this blog post: it is always better to use a contract priced in native currency instead of using a bonding curve.

Bonding curves have a couple major limitations in comparison to contracts priced in the native currency:
* bonding curves have exchange rate risk. The amount of volatility is generally in proportion to the market cap of the currency. A bonding curve creates a new subcurrency with a market cap much smaller than the native currency, so the volatility will be high, which means the exchange rate risk is high. This problem would not occur if we used a contract priced in the native currency instead.
* bonding curves create a sub-currency, so they have to have major on-chain components. This makes it expensive because of gas costs, and it ruins privacy. If we used a contract priced in the native currency instead, then we could put the contract entirely off-chain in the lightning network.
* if the on-chain market to swap native currency for subcurrency uses single price batches, then the gas price will be very expensive for users, in comparison to a contract priced in native currency.
* if the on-chain market does not use single price batches, then it is vulnerable to front-running from the miners, and this will make the bonded curve very expensive, in comparison to a contract priced in native currency.

Bonding curves have no advantages over smart contracts priced in the native currency. 


Now lets look at some specific examples of goals that bonding curves could supposedly be used for, and I will show that there is always a better way to make a contract priced in the native currency.
* A bonding curve can have a resolution that kills the contract and distributes the native currency in it to everyone holding the subcurrency.
  - this mechanism does not work. depending on the curve used, a person who sells their subcurrency to the bonded curve contract before it expires will get more than 2x as much native currency, compared with a person who waits for the contract to expire and distribute all the Eth. So everyone will rush to the exit before the bonding curve ever has a chance to distribute the native currency. Whoever leaves last will lose almost everything that they invested. 

* A bonding curve can be used to raise money for a goal like creating a public good.
  - A lot of people are making this claim, but no one has gone into specifics. It is not clear at all how the bonding curve could raise money for a public good. If we took all the native currency out of the bonded curve contract and used it to pay for the public good, then it would break the bonding curve. You would no longer be able to sell your subcurrency to the contract for native currency.
  - There are already great designs for contracts priced in the native currency to raise funds for public goods. They have even done math to show that it is incentive compatible. Check out page 14 of this pdf: http://bitcoinhivemind.com/papers/3_PM_Applications.pdf

* A bonding curve can be used to create a subcurrency with some utility, like gambling or a prediction market or a quiz.
  - You can use native currency to do any of these same things. There is no benefit for creating a subcurrency for this, and there is significant cost for creating a subcurrency for this.
  - if someone makes a gambling contract that only accepts subcurrency, there is nothing stopping anyone else from cloning it to make a version that accepts native currency.

* Bonded curve subcurrency could be used for voting.
  - Voting cannot be a secure protocol for making decisions. It is vulnerable to tragedy of the commons. It is vulnerable to bribery. https://blog.ethereum.org/2015/01/28/p-epsilon-attack/
  - We already have tools like futarchy which are ideal for making decisions. see page 8 of this pdf http://bitcoinhivemind.com/papers/3_PM_Applications.pdf
  - Restricting participation to only people who own a subcurrency makes the decision less accurate. Markets have more accurate prices if more people can participate, so it is better to use native currency for decision markets instead of using a subcurrency.

* Steemit style multi-level-marketing schemes
  - With bonding curves, the total value of Eth being stored in the contract is significantly less than the notional value of all the subcurrency produced. So if the contract shut down and distributed it's Eth, each participant would receive Eth worth about 1/2 as much as the subcurrency they lost. So everyone is incentivized to sell all their subcurrency before the bonded curve contract shuts down. This causes a big rush to the exit, and whoever leaves last loses almost everything they invested.
  - With a bonding curve there isn't anything connecting the success of the subcurrency to the success of the product being marketed. If we used a normal smart contract instead, then an oracle could tell the smart contract about how successful the product is, and the smart contract could reward participants accordingly.
  - It would be much more effective to make a normal financial derivative and bet on how much product gets sold in the next month. Then the people producing the product could buy the opposite side of your contract to hedge their risks. Financial derivatives have been used this way for longer than written history has existed. 




Now lets take this more abstract.

Blockchain is a zero sum game. Your side of a contract can only gain value if the other person's side loses value.
Blockchain can be used for these applications: buying/selling information, for hedging risks, for storing value, or for sending value.
Hedging risks is similar to buying insurance. You don't buy insurance to get rich, you buy it to lower the probability that you will end up poor.
Incentivizing people to reveal private information can be useful when we want to make big important decisions, and we can't risk making a bad decision.

If someone tries to advertise a blockchain tool, and they claim that it has an application other than hedging risk or trading information or sending/storing value, then it is a scam.


Sub-currencies are always the wrong design choice. They increase exchange rate risk, increase volatility, and prevent channel-type scalability. It is always better to use contracts priced in the native currency.

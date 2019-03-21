The killer app of blockchain is a scalable market to trade assets who's price is determined by a trust-free and affordable oracle.
Amoveo attempts to achieve this goal by keeping the oracle on-chain, and moving the market off-chain onto the channels.
The market allows for limit orders. That means you can tell the market
"I really want to buy tomatoes, but only for less than $1 per kilo. I will leave my money
here, if the price ever goes below $1 per kilo, then please buy the tomatoes. I will come back later to either pick up the tomatoes, or my money."
The market will honor your trade, and buy the tomatoes for you.

Since this is a smart contract, you do not have to trust the person running the market.
It is a trust free relationship. There is zero risk of them taking your money.
Since it is off-chain, you don't have to pay a transaction fee, or wait for any confirmations.

Today these services are provided by centralized people or companies that you have to trust. The only way to find people trustworthy to not take your money is if you pay them a high enough salary so that they prefer the long-term profits of the salary over the short term profit of robbing you.
This makes it very expensive.
Additionally these centralized companies have to comply with expensive regulations.
The demand is so high, no public blockchain can fit all this trading data on-chain.
Providing these markets off-chain will be the killer app of blockchain.

## What does an off-chain smart contract for a market look like?

You make a channel with an account that manages a market.
Everyone who wants to trade is making channels with this same account.
Your current channel state looks like this:
```
You have: $1000, partner has: $500, contract has: 0, contract code: "".
```

You want to buy shares of a synthetic asset linked to the value of gold.
You only want to make this purchase if the gold costs less than $1280 per ounce.
So, you make a channel with a contract like this and both sign it.

```
You have: 0, partner has: $2,
contract has: $1498, contract code: "
    The channel can only be closed 
    at this state if the market manager 
    has signed over a current trading 
    price, and it is =< $1280 per ounce.

    If you can show that the market 
    manager signed over 2 contradictory final prices, 
    then you can take all the money 
    from the channel, and the market 
    manager gets nothing. 

    If you try closing at different final
    states that the market manager signed, 
    the blockchain will prefer the earliest
    valid one.

    Y = final price of gold per ounce that the market manager chose.
    X = 998 / Y,%this is the number of ounces of gold you own.
    Z = oracle("price of gold ounce"),
    B = X * Z,
    The first B dollars go to you, any remaining money goest to your partner.
"
```

## Is it secure according to game theory?

One goal is to make sure that the market manager can only choose a price once in each batch. This is important to prevent front-running. 
This way the market maker sells the shares for the same price he is buying them for.
The contract can let his customers take all the money if the market manager signs on contradictory forks.

```
A: If the market maker can only choose one price per batch
B: The market maker will choose the most optimal price.
To prove: if A, then B.
```

If the market maker chooses the optimal price, he isn't changing his risk.
He is earning a profit by taking a fee.
This graph shows how if he selects the correct price the sells in blue match the buys in black so he doesn't risk anything.

![balanced trading](batch_channel.png "balanced")

Here is what it would look like if the market maker choose the incorrect price. He ends up selling many more sells than he purchases. He takes on a lot of risk for very little reward.

![unbalanced trading](batch_channel_unbalanced.png "unbalanced")

therefore, `if A, then B` is proved.



To prove it is secure, we also need to check the case of [censorship](censored_orders_in_channel.md)

Some people think that it is a drawback that this market design uses batches instead of accepting trades instantly.
Markets can only be secure if batches of single price are used.
Casey Detrio explains it well.
https://www.youtube.com/watch?v=mAtD0ba-hXU

Gnosis also attempts at explaining:
https://blog.gnosis.pm/introducing-the-gnosis-dutch-exchange-53bd3d51f9b2
[Here is a copy of the text of the gnosis dutch exchange post](../other_blockchains/gnosis_dutch_exchange.md)

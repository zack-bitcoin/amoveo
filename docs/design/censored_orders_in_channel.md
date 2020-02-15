WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//design/censored_orders_in_channel.md)

problem: what if the market maker refused to add any orders to the market based on the price of the order?

Each market should advertise a certain size trade, which is very small.
All the traders should only make trades in this small size.
If you want to buy shares of type 4 in a market that has 4 types, you should also buy and sell a few shares of the other 3 types to verify that the oracle isn't censoring any trades.

What if the channel path isn't completed, and someone in between is censoring your trades?
Every step of the channel path should be formed using only the hash of the contract, that way the participants in the path don't know what they agreed to until after they have agreed.

For questions that are in the process of being answered, we store a market in the on-chain state.
The market remembers how many shares of each type have been sold, and it remembers what it's initial liquidity was, and it should have an order-book.
The market has 4 possible outcomes:
1) difficulty goes up, and oracle's outcome is true
2) difficulty goes up, and oracle's outcome is false
3) difficulty goes down, and oracle's outcome is true
4) difficulty goes down, and oracle's outcome is false


The initial liquidity will be collected using an off-chain dominant assurance contract.

the result of the oracle is determined by which side of the order book has open orders. If one side has open orders for a long enough period of time, then that side wins.

Since we use an order book, it is expensive for attackers to DDOS us by moving the price past 50% every time the oracle is almost done.


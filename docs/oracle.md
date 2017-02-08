We use a trie to store all the questions that have been asked of the oracle.
We use another trie to store the answers, for any questions that were answered.
We use a trie to store all the questions that have been asked of the oracle.
We use another trie to store the answers, for any questions that were answered.

For questions that are in the process of being answered, we store a market in the on-chain state.
The market remembers how many shares of each type have been sold, and it remembers what it's initial liquidity was, and it should have an order-book.
The market has 4 possible outcomes:
1) difficulty goes up, and oracle's outcome is true
2) difficulty goes up, and oracle's outcome is false
3) difficulty goes down, and oracle's outcome is true
4) difficulty goes down, and oracle's outcome is false



***  Somehow, we need to know how many shares of each type each account owns.

The initial liquidity will be collected using an off-chain dominant assurance contract.

the result of the oracle is measured by looking at the correlation between the outcomes.
We sum the diagonals, and see which way is bigger.
If the correlation is in the same direction for enough blocks, then that is the oracle's result.
Since we use an order book, it is expensive for attackers to DDOS us by moving the price past 50% every time the oracle is almost done.


We should pay the better all at once, because only one fork can survive.
When a better wants to get paid, they need a way to prove how they bet.
*** Maybe we should have a merkle root of bets in each account?

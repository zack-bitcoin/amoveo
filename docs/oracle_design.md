We use a trie to store all the questions that have been asked of the oracle.
We use another trie to store the answers, for any questions that were answered.

For questions that are in the process of being answered, we store a market in the on-chain state.
The market remembers how many shares of each type have been sold, and it remembers what it's initial liquidity was, and it should have an order-book.
The market has 6 possible outcomes:
1) difficulty goes up, and oracle's outcome is true
2) difficulty goes up, and oracle's outcome is false
3) difficulty goes down, and oracle's outcome is true
4) difficulty goes down, and oracle's outcome is false
5) difficulty goes up, and the oracle's outcome is bad_question
6) difficulty goes down, and the oracle's outcome is bad_question

Each market should have a trie for storing orders in the order book, and it should have an accounts trie, to remember how much money each account has.

All the market trie roots should be combined as a merkle trie into the block, so that as the number of markets grows by N, the increase in cost is only O(log(N)).

in total, that was 5 tries:
* questions. each block has a root
* answers. each block has a root of this
* order book trades
* how many shares each account owns
* a trie that stores pairs, containing one each of the previous 2 types of trie roots, an order book trie root, and a shares-accounts trie root. each block has a root of this.

The roots in each block should combine to make a single root that is stored in the block hash.



the result of the oracle is measured by looking at the correlation between the outcomes.
We sum the diagonals, and see which way is bigger.
If the correlation is in the same direction for enough blocks, then that is the oracle's result.
Since we use an order book, it is expensive for attackers to DDOS us by moving the price past 50% every time the oracle is almost done.


tx types:
AskQuestion
OracleBet
OracleFinish

AskQuestion
This costs a big deposit. If the question is answered in the expected way, you get most of the deposit back.
If they decide it is a bad question, then your deposit is deleted.

OracleBet
This is how you make a bet in the on-chain market, which measures the correlation between the answers to oracle questions, and the mining difficulty. It is a LMSR order book, so you say how many shares for the lowest price you are willing to sell at, and you say how many shares for the highest price you are willing to buy at.

AnswerQuestion
If the oracle's correlation has shown with high enough margin for a long enough period of time that one answer is preferable, then anyone can do this transaction to permanently record the answer to the question.




The initial liquidity will be collected using an off-chain dominant assurance contract.
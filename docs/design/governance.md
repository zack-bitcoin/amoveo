Using the oracle, the parameters that define the system can be modified. The mechanism is built with the goal that the oracle will choose parameters that make the value of Veo increase.

We have an oracle that asks "should this variable be pushed up or down?". Depending on the final state of the oracle, the variable is changed.


The parameters that can be modified are of 4 different types:
1) blockchain variables
2) virtual machine variables
3) oracle variables
4) transaction fees


1 blockchain variables
======

* block reward - how much reward the miner gets for finding a block.
* developer_reward - developers get this reward when a block is mined.
* max block size - this is the limit on how big a block can be.
* block period - this is the average amount of time between when 2 blocks get mined.

2 VM variables
======

* time gas - this is a limit on how many CPU cycles can be used to compute the outcome of a smart contract.
* space gas - this is a limit on how much ram a smart contract can use.
* fun limit - this is a limit on how many functions a smart contract can define.
* var limit - this is a limit on how many variables a smart contract can define

3 Oracle variables
======

* governance_change_limit - this is a limit on how much a governance variable can be changed at once.
* oracle_initial_liquidity - how much does it cost to launch an oracle.
* minimum_oracle_time - This is the minimum length of time an oracle market is required to be in a single state before it can be closed.
* maximum_oracle_time - This is the maximum delay between when an oracle is created, and when betting can start.
* maximum_question_size - how many bytes big can a question for the oracle can be.

4 Transaction fees
======

Each of the 15 transaction types has a fee who's size is determined by a governance variable.
This fee does not go to the miner, it is deleted.
The miner fee is a different fee from this one.

[The transaction types are listed here](transaction_types.md)
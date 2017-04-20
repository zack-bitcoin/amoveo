The future price of blocks is split into P possibilities. between 3 and 20.

The future value of oracles is split into 2 possibilities.

The future value of governance variables is split into G possibilities. between 300 and 100000.

We will use a market to make the P types of shares.
We will make P markets for each oracle, priced in each of the P shares.
We will make P markets for each governance variable, priced in each of the P shares.

standard LMSR.
C = b*ln(Sum[i from 0 to 1-P](e^(q_i/b)))


At each block, users can bet in whichever markets they want.
C blocks later, we do some statistics and determine the winners and pay them. Winners prove their bets to win, consensus only holds the merkle root of their bets.
Unfortunately, we can't pay the winners slow and exponentially, because it is too computationally expensive to read every block since C ago, and give out rewards for every bet.
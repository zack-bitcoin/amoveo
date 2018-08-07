Cryptoeconomics is the field of study that uses cryptographic tools and economic mechanisms to enforce the rules of a protocol. It is a form of mechanism design.

When we say that a mechanism is "cryptoeconomically secure", this means that we have cryptoeconomic guarantees that the protocol will have certain properties.

In order to make proofs feasible, the community has focused on building mechanisms where the default behaviour is also a nash equilibrium.

If the default behavior is a nash equilibrium, then we have a cryptoeconomic guarantee that nearly everyone will use the default strategy to participate in the protocol.

Lets look at an example.
Your mining pool controls some portion of the hashpower <50%. They have 2 binary decisions to make for how they participate in the protocol. Which makes 4 possibilities: True/True, True/False, False/True, False/False. Which we will write as 2 bits: 00 01 10 11.
Lets assume that the default strategy programmed into the mining pool is 01.

This is an example payout matrix showing how much your mining pool would get paid in each situation.
x-axis is your choice, y-axis is the mode of the network hashpower's choice.
```
Matrix 1.
    00  01  10  11
00  0   0   0   0
01  4   12  5   15
10  100 0   0   0
11  20  1   1   1
```
This matrix shows that the default strategy of 01 is not a nash equilibrium. 
If the network chooses 01, then it is in your interest to choose 11 so you can earn 3 more.
So it is in every mining pool's interest to change the rules to earn a profit.
Since the default strategy of 01 is not a nash equilibrium, this might not be a cryptoeconomically secure protocol.

Now lets modify the payout matrix into one where 01 is a nash equilibrium
```
Matrix 2
    00  01  10  11
00  20  0   0   0
01  4   12  5   1
10  100 0   0   0
11  20  1   1   1
```

strategy 01 pays 12 which is the biggest payout in row 01, so that means strategy 01 is a nash equilibrium strategy.

Notice this payout matrix has 2 nash equilibrium. one at 00, and one at 01.
So either 00 or 01 could be a secure default strategy.

So far we have only considered 2D matrices.
A mining pool is making many more than 2 binary decisions, so when checking the security of a protocol, the matrix is larger. The number of decisions is as many as the minimum number of bits needed to program the default strategy.

When designing protocols you often need to be aware of large parts of this matrix.
Luckily, when verifying a protocol, we only need to look at 1 row of the matrix. The row for the default strategy.

Looking at Matrix 2, since the default strategy is 01, we only have to consider this row when verifying the security:
```
4 12 5 1
```
the 01 decision is worth 12, which is the biggest in the row.
So strategy 01 is a nash equilibrium.
So this is a cryptoeconomically secure mechanism.

A nash equilibrium can only exist on the diagonal of the matrix. All the other spots mean that you are incentivized to use a non-default strategy.
When looking for a nash equilibrium, you want to find a row such that the intersection of that row and the diagonal is the highest payout strategy in that row.


Now lets look at some more concrete examples:

1) mining on the default branch D vs mining on a fork to cause a double-spend F. 
You can win 450 by doing the double-spend. you can win 50 from mining rewards by mining on the winning side.
```
    F   D
F   500 450
D   0   50
```

D is the default strategy, and it is a Nash equilibrium, because it is the highest number in its row. 50 is bigger than 0.
Since D is a nash equilibrium, and it is the default strategy, we have a cryptoeconomic guarantee that the protocol will decide on D, not F.


2) voting. We will all be 50 richer if we can decide on G. We get nothing if we decide on B.
```
    G     B
G   50    50
B   0     0
```

This matrix has no nash eqiulibriums. G:G is 50, which is not the lowest in the row. B:B is 0, which is not the lowest in the row.
Since there are no nash equilibriums, voting is not a cryptoeconomically secure mechanism.


3) voting where we will all be 50 tokens richer if we can decide on G, we get nothing if we decide on B, there are 100 voters, and there is an adversary who is willing to bribe 1 token per voter to pay them to vote on B.
```
     G    B
G    50   51 
B    0    1
```
This matrix has a nash equilibrium on B, and no nash equilibrium on G.
So even if the default behavior was to vote for G, instead the community will switch to voting for B.
The adversary can use 100 tokens to destroy 5000 tokens.

4) futarchy. You can bet in 2 directions. H = (Network makes decision H and the token value goes up OR we make decision L and the token value goes down), L = (we make decision H and the token value goes down OR we make decision L and the token value goes up).
Lets assume that decision H is preferable, and it actually causes the token value to rise.
If you bet correctly, you earn 100. if the network chooses decision H, then you earn 10 from the token value increase.
```
      H     L
H     110   10
L     100   0
```

There is only one nash equilibrium. It is on H.

Therefore, we have a cryptoeconomic guarantee that the network will make the better decision.

Which means that futarchy is a cryptoeconomically secure mechanism for making good decisions as a group.

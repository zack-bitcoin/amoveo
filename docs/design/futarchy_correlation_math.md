The math behind futarchy
============

P(A) means "the probability of A being true."
P(!A) means "the probability of A being false."
P(A|B) means "the probability of A, given that B is true."
P(A and B) means "the probability that both A and B are true."

First a few tautological conditional probability facts:

1) P(A) = P(B)P(A|B) + P(!B)P(A|!B);
2) P(!A|B) = 1 - P(A|B);
3) P(!A) = 1 - P(A);
4) P(C and D) = P(D)*P(C|D)
5) P(A|B) - P(A|!B) = P(!A|B) - P(!A|!B)

Definitions:
if A and B are "independent variables", then:
P(A|B) = P(A|!B),
P(B|A) = P(B|!A),

When we want to know whether to update, we use futarchy to find out if the update will have a positive or negative influence on the price.

C = The price of Veo is up
D = we did an upgrade

We ask the prediction market about the relationship between C and D to know if we should update.
One type of relationship we could measure is the correlation between the variables.

Correlation = P(C and D) + P(!C and !D) - P(!C and D) - P(C and !D)
If the correlation is >0, then the update will have a positive influence on the price. If it is <0, then it will have a negative influence on the price.

Another relationship between C and D that could be used as a futarchy metric is:
P(C|D) - P(C|!D). This is measuring how much more likely we are to achieve our price goals if we do the update.




Some people worry that factors such as the background adoption rate of Amoveo could interfere with the outcome of a futarchy market.
Here we prove that this isn't a porblem.

X = is adoption rate fast.
A = price goes up
B = we do a hard update

Our futarchy formula:
P(A|B) - P(A|!B) %this measures how doing the upgrade increases the odds of achieving our goal.

P(!A|B) - P(!A|!B) % this measures how doing the upgrade increases the odds of failing our goal

```
Add in the X factor:
=P((P(A|B) - P(A|!B)) | X)
Simplify by setting Z = P(A|B) - P(A|!B)
=P(Z | X)
Since X is independent of A and B
=P(Z | !X)
Summing the previous 2 formula, and dividing by 2:
= (1/2)*(P(Z|X) + P(Z|!X))
tautology (1)
= P(Z)
Simplify by setting Z = P(A|B) - P(A|!B)
= P(A|B) - P(A|!B)
```

As you can see, the X completely cancels out. No matter what value we set for X, the same result is calcualted.
So the rate of adoption does not make any difference to the result of the futarchy market.

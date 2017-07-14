Blockchains allow for prediction markets, which let us prepare for the future.
To show how powerful prediction markets can be, we will use a prediction market to play go against a professional, and we will win.

First we will make the big market, it sells 2 kinds of shares. "AI" shares only pay out if the professional loses. "Human" shares only pay out if the professional wins.

After every move, the relative price of "Human" and "AI" shares will change.
For each move, we make 2 prediction markets, each with <=381 possible choices. The only difference between the two markets is that one is priced in "Human" shares, and the other in "AI" shares.

So for example, lets say the current price is 60% likely that the human will win,
and you think that if the AI plays 5C, that the odds will shift to 50% likely that the human will win.
(Which would make your AI tokens 5/4 as valuable)
You also think that if the AI plays anywhere besides 5C, that the odds of the human winning will increase to 80%.
(which would make your AI tokens 1/2 as valuable)

There are 4 locations in the Nash square defined by 2 questions:
1) Does the AI play on 5C?
2) Does the AI win?

You don't bet on either of these questions directly, rather you bet on the correlation.
You would buy shares of (yes, yes), and you would buy shares of (no, no). You would sell shares of (yes, no) and (no, yes).
You keep doing this until the prices of shares are such that if the AI plays 5C, the price of AI shares increases to 50%, and if the AI plays anywhere else, the price of AI shares decreases to 20%.





To decide the initial values of the <=381 outcomes for each market, we use the final values from the previous round of betting. Most of the good moves are still good, most of the bad moves are still bad.
## Radar Relay Review

Radar Relay is a company that is trying to use 0x technology to build markets for trading ERC20 tokens on Ethereum.

Their stated goals are similar to what Amoveo does. So I wanted to find out if they are actually achieving these goals. Maybe we could re-use some of their techniques to make Amoveo better.





So I tried to contact them to find out how much of their market is off-chain vs on-chain, and how they prevent front running.

During my discussion, it became apparent that Radar Relay has no security to prevent theft by front running.
Once I started explaining how allowing customers to get robbed is a serious problem, I was blocked from their group.

Here is a video about the dangers of front running:  https://www.youtube.com/watch?v=mAtD0ba-hXU

### Below is our discussion.


Zack-
I wrote a trustless off-chain order book. I used programmable channels to enforce the rules to make it trustless.

Relayer.network also has an off-chain order book.
I am wondering if your order book is trustless, and if so, how you enforce the rules?

Whitneyadmin-
We host the order book off chain. All the orders are settled on chain and through the app or through the API  users can look at the hash of any order. It's a little technical but you can verify the contract behind the proposed transactions (open orders).
We standardize the orders that are allowed on the order book. If it doesn't fit the format it's not part of the order book.

Zack-
How do you make the order book trustless?
Or is it a trustful centralized order book?
How do I know that you will match my trade honestly, without any front running?

Whitneyadmin-
We don't match your orders. We don't have a central matching engine.

Zack-
so there is a centralized server for matching orders?
How can I know that this server won't let anyone front-run my trades?
In the off-chain market that I wrote, I used a smart contract inside the state channels to enforce the correct matching of trades to prevent front running.
It uses single price batches.

Whitneyadmin-
There is not a central server for matching orders. We use an open order book.
If you place an order on Radar that crosses the spread then it will show a negative spread.

Zack-
ok, so how do your enforce the correct matching of trades to prevent front running?
do you use single price batches? or a commit-reveal system?

Whitneyadmin-
There is no central matching engine. We do not match orders.

Zack-
the purpose of Radar Relay is for people to trade ERC20 tokens, correct?
So there must be some market with an order book connecting the traders together.

Whitneyadmin-
Right now you can use individual fills by specifically selecting an order or a 'fill up to' function. The second being a market order.

Zack-
so you do match orders between people.
Do you have cryptoeconomic guarantees that the orders are being matched fairly without front running?
It is bad to use a dapp if there is no cryptoeconomic guarantees to prevent theft.
If it is not secure against theft, and the other participants in the dapp are anonymous, then it is almost guaranteed that theft would happen.
I can wait if you need to ask someone else how this works. I understand that not everyone on a team knows all the security details.

Whitneyadmin-
When you sign an order and send it to the chain you are signing a specific hash. This is mined on chain. The order hash you signed is sent with lower gas and someone else fills that same hash the order will not go through. That's an order collision, which sometimes gets mistaken for frontrunning.

Zack-
so orders are matched on-chain?
but earlier you said that the order book is off-chain.
can I see the on-chain smart contract you are using for the order book?

Whitneyadmin-
**With an open order book you can have front running.** It is technically possible. Will Warren had a great article about this. If you would like.

The order book is hosted off chain. All orders are settled on chain and there is no central matching engine.
If you would like to see the design:
https://developers.radarrelay.com/learn-more#sra

Zack-
So you have no front running security at all?

Why not?
The off-chain order book that I programmed has front-running security.

I can't understand why a person would use your market if they can get robbed.

Whitneyadmin-
This sounds like you might be expecting a central contract, like a smart contract custody exchange. We use 0x smart contracts. To learn more about that start here: https://0xproject.com/

0Xproject
An Open Protocol For Decentralized Exchange On The Ethereum Blockchain

I t sounds like you are very interested in getting people interested in what you've built. If you would like us to evaluate some code I'm sure one of our engineers would have a look out of curiosity. But I can't promise a fast turn around on that.

Check out Will Warren's article on front running
https://blog.0xproject.com/front-running-griefing-and-the-perils-of-virtual-settlement-part-2-921b00109e21

0x Protocol
Front-running, Griefing and the Perils of Virtual Settlement (Part 2)
This post is a continuation of part 1, which provided an introduction to blockchain race conditions, front-running, the 0x protocol…
More than anything right now there is a chance that **if you place orders with low gas someone can fill that order first. This can be disappointing.**

In the right context there is a very small chance of getting frontrun. Not only are we working to eliminate that chance with future design changes we are also watching all the orders that go through Radar and we can see that it isn't happening. Order collisions are rare, that's the closest thing to it and people often confuse the two.

Zack-
I am thinking of writing a review about Radar Relay.
Is there anyone else who can explain the security in regards to front running?
Because right now the review is going to be bad.

I have spoken with the 0x team, and I have read the paper you link to.
0x is just a protocol for trading ERC20 tokens, it is not an order book to prevent front running.
If you want to use 0x for a secure market, you need to make the order book separately.

The paper you linked to describes a couple strategies to prevent front-running with 0x.
One of which gives all the power of front-running to a central authority.
Another moves the order book on-chain.

Is Radar Relay using one of these strategies? which one?
Radar Relay is being described as off-chain relay and on-chain settlement.
So is the order book off chain or not?



At this point I was banned from their forum.

### conclusions

There are 3 important properties for blockchain markets:
1) trustless (without this, the person running the market can steal from customers.)
2) trades at the market price (without this, traders can steal from each other by front-running and arbitrage.)
3) off-chain (without this, it cannot scale)

Ethereum can have any 2 of these at a time, but only Amoveo has all 3 at once.

Shapeshift gives (2) and (3).
0x gives (1) and (3).
Augur and Gnosis gives (1) and (2).

At first glance, Radar Relay advertises itself as if it can achieve all 3, but upon closer inspection you will realize that it cannot.

As Whitney said, high-gas orders can front-run low-gas orders.
Since it is possible to front-run, they don't have (2).

So Radar Relay cannot prevent theft from traders by front running and arbitrage.
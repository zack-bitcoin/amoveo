Governance decides a different minimum fee size for each transaction type.
The miner profits by including transactions that pay above the minimum fee for that transaction type.

These are the 15 types of transaction that can be in blocks.

3 transactions for accounts:
* create_account_tx
* spend_tx
* multi_tx

5 transactions for channels:
* new_channel_tx
* channel_team_close_tx
* channel_solo_close
* channel_slash_tx
* channel_timeout_tx

5 transactions for the oracle:
* oracle_new_tx
* oracle_bet_tx
* oracle_close_tx
* oracle_unmatched_tx
* oracle_winnings_tx

1 bonus transactions:
* coinbase_tx

# create_account

This creates a new account on the blockchain and gives it some tokens.

# spend_tx

Spends tokens to a different account.

# account_delete

This deletes an account on the blockchain and sends all of it's Veo to a different account.

WARNING! do not reuse a pubkey after it has been deleted.

# multi_tx

A multi-tx contains multiple create_account and spend_txs inside of it. A multi-tx only updates your account nonce once, this makes it ideal for cold storage.

# channel_new

This creates a new channel on the blockchain.
It needs to be signed by both participants in the channel.
It takes money from both participant's accounts to put into the channel.
Channels can only hold Veo tokens.
There is a record of recently closed channels. You can't reuse an id from a recently closed channel.

# channel_team_close

Both parties need to sign.
This closes the channel.
The tokens in the channel are distributed to the 2 account owners.

# channel_solo_close

If your partner disappears, or refuses to close the channel, this is how you can start the process of closing the channel without your partner's help.

# channel_slash

If channel participant does a channel_solo_close at the wrong state, this is how you stop them.
Anyone is allowed to publish this tx, it doesn't have to be one of the two channel participants.
The blockchain records who closed a channel most recently in a tree.
channel ids can't be reused for a long time, so the record will be good.
The channel can be slashed many times, but each time it is slashed the evidence needs to be for a higher nonce.

# channel_timeout

If you did a channel_solo_close, and then waited the delay number of blocks after the final channel_slash, now you can do this transaction to close the channel.

# oracle_new

This asks the oracle a question.
The oracle can only answer true/false questions.
Running the oracle costs a fee which is used as a reward to get people to use the oracle.
The fact that an oracle exists is recorded on the blockchain in a way that is accessible to the VM. So we can use channels to make smart contracts to raise funds to run the oracle.
The entire text of the question is written into the transaction, but only the hash of the text is stored into a consensus state merkel tree.
The oracle has a start-date written in it. Trading doesn't start until the start-date.
The oracle can be published before we know the outcome of the question, that way the oracle id can be used to make channel contracts that bet on the eventual outcome of the oracle.

# oracle_bet

This is how you can participate in an existing oracle.
The market is an order book with 3 types of shares: "true", "false", "bad_question"
All trades are matched into the order book in pairs at even odds.
So the order book only stores 1 kind of order at a time.
If you want your order to be held in the order book, it needs to be bigger than a minimum size.
The minimum size gets bigger as the order book gets bigger. So if there is C number of coins in the order book, the number of orders is smaller than `log2(C/(minimum for first order))`
If your order isn't big enough to be in the order book, you cannot buy shares of the type that are stored in the order book.

# oracle_close

If there is a lot of open orders for one type of share in an oracle for a long enough period of time, then this transaction can be done.
This ends betting in the market.
The fee that was used to start the oracle is the final bet included. It bets against the winning outcome.

# oracle_unmatched

If you had money in orders in the oracle order book when the oracle_close transaction happened, this is how you get the money out.

# oracle_winnings

If you bet in an oracle, and the oracle has closed, this is how you get your winnings out.
If you bet on the winning outcome, then you get twice as much money back, otherwise you get nothing.


[transaction types are the ways to modify blockchain consensus state. All the consensus state is stored in trees. Read about the trees here](trees.md)
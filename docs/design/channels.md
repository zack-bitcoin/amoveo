Publishing a transaction to a blockchain is time consuming. You need to wait for the transaction to be included in a block, and then depending on how much money is moved, you need to wait for more confirmations. Waiting for confirmations means that you wait for enough blocks to be added to the blockchain for enough security. Trying to avoid this time-consuming process as much as possible is why we use channels.

Channels are 2 party relationships recorded on the blockchain. Each channel has a finite amount of money it controls, just like an account.
Once 2 people have a channel together, they can instantly move the money inside the channel back and forth. This is much faster than publishing a transaction to the blockchain and waiting for confirmations.
When the channel is closed, the money goes back to the accounts that created it according to the final distribution of money in the channel.

[here is an explanation of the data stored for channels.](channel_state.md)


There are 5 transaction types for controlling channels:

2 that need to be signed by both participants: channel_new, channel_team_close.
3 that are signed by one: channel_solo_close, channel_slash, channel_timeout

If your partner disappears, and you want to close the channel, then you first publish a solo_close, which gives the current state of the channel, which is defined by a turing complete contract that outputs a nonce.
The state of the channel includes contracts, which are split into 3 parts, the scriptpubkey which both participants signed is last, and the scripsig part, which only one signed is the first part. For the  middle  part, the blockchain looks things up in the merkel tree, and provides that data to the smart contract too. This is used when you are betting on the outcome of something in the oracle, for example.

If your partner sees you publish a solo_close that doesn't output the highest nonce possible, then they can do a channel_slash transaction that uses a different script sig, so long as the new script sig causes the contract to output a higher nonce.
If there is another script sig that could cause an even higher nonce, then it is possible for more channel_slash transactions to get made that changes the nonce again and again.

Eventually when enough time passes without a channel_slash, it becomes possible to do a channel_timeout transaction, which closes the channel at the final state.

The data that gets recorded on-chain for each channel is:
How much money is in the channel. The 2 accounts ids that control the channel.

channels are stored in one of the consensus state merkle trees. [read more about these trees here](trees.md)
There are 5 transaction types:
2 that need to be signed by both: channel_new, channel_team_close,
and 3 that are signed by one: channel_solo_close, channel_slash, channel_timeout

If your partner disappears, and you want to close the channel, then you first publish a solo_close, which gives the current state of the channel, which is defined by a turing complete contract that outputs a nonce.
The contract is split into 2 parts, the scriptpubkey part, which both participants signed, and the scripsig part, which only one signed.

There is a third part that goes in the middle. The blockchain looks things up in the merkel tree, and provides that data to the smart contract too. This is useful if you are betting on the outcome of something in the oracle.

If your partner sees you publish a solo_close that doesn't output the highest nonce possible, then they can do a channel_slash transaction that outputs a higher nonce, and this becomes the final state that the channel gets closed at.

If your partner doesn't slash you, then eventually you can do a channel_timeout transaction, which closes the channel at the state from the solo_close.

The data that gets recorded on-chain for each channel is:
How much money is in the channel. The 2 accounts ids that control the channel.
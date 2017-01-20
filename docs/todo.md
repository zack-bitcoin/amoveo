right now channels are charging rent in channel.erl, but not when we close the channel.
We need to update the channel before closing it.

If one of the channel participants runs out of money, then the channel can't ever be closed.
Instead, the person with negative money should have zero money, and the person with extra money shouldn't have extra.



channel_test.sh needs work.
It needs to make a bet.

We should use a CLI program to talk to the node instead of using erlang directly.

We need to update download_blocks so that peers get ranked, and we spend more time talking to higher-ranked peers.
There is a problem where if you crash while syncing with a peer, then you skip trying to sync with any peer lower on the list. this is very bad.

block:check2 needs an update. It should only accept blocks that were made after the median of the last 100 blocks

make the api networking/handler be entirely encrypted. This is to protect information about the channels.


we need a channel powered satoshi dice for gambling. look at channel_test.sh

we need to re-write the channel manager stuff for the new channels.

download_blocks could be more efficient.


maybe nodes need to advertise their own IP/port combo as a peer?





Updates for next time we restart at a genesis block:

Newly mined coins should not be spendable until they mature. Bitcoin waits 100 blocks for maturity.

proof of existence transaction type.

each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.

blocks should point to the previous header, not the previous block.

Mining should be on headers, not on blocks.

We need to reward the miner with the transaction fees, to incentivize him to include them. block:absorb_txs

making a channel should require both parties to sign, that way attackers can't trick servers into dropping their channel state.
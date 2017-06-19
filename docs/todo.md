Prices listed are minimums. If the code is high quality, you can get much more.

off-chain markets. $2000
We need an integration test where one node is a market, and the other two nodes are traders. 

There should be a way to start the node in lite-mode. So that it only downloads headers, not full blocks. $1000

download_blocks:get_blocks should download multiple blocks at a time. $100

We need to test channel_solo_close and channel_slash and channel_timeout from easy. $30



Cold storage. $150

trees:garbage needs to garbage collect more trees. $100

we should optionally garbage collect old blocks, only keep the headers. $400

We should let a node sync with the network without downloading all the old blocks. Instead it downloads headers, and a recent state tree. $300


spk:is_improvement needs better checks. $50
Make sure delay isn't too big, and the fees aren't too high. 

We should seperate all the consensus code from the non-consensus code. This way soft forks will be easier. $150
Mostly this means moving all the channel stuff into /src/channels
stuff from spk for example.

We need a way to know how long the delay could be on every possible way of closing the channel.
The channel can only close on a ?crash opcode, the delay is always programmed in just before the ?crash, so we should be able to calculate the delay from the bytecode.
Only the script_sig part of the contract can have a ?crash opcode, so this is something we can calculate.
spk:is_improvement
We need to regularly check on our channels to see if either participant is running out of funds. There needs to be enough money left to cover the cost of the channel for the amount of time until you can close the channel without your partner's help.
When you are running short on funds you need to ask your partner to close the channel. If they don't, you need to start closing the channel without their help.


in spk prove_facts2, the burn and existence trees store by hash not by integer, so the code needs to be modified for them. $60



download blocks talk/1 seems useless. talker:talk is accomplishing the same goal. $40

It is possible to use channel_slash to store data in a channel such that the channel can't be closed with a channel_timeout.
Maybe this is a mistake.

javascript VM implementation $1500

javascript light wallets need to be able to do all the channel stuff that full nodes do. $2000


It is currently possible for an attacker to trick us into ignoring a good block. They trick us into storing a good blocks hash into block_hashes. They give us a good block's header, but mix in some bad transactions, or censor a transaction, or they don't give us some of the merkel tree we need to verify the transactions. $1000

constants:difficulty_bits() might be too big.


We need to also add a way for the two parties to work together to close the channel early, so they don't have to wait to do a timeout_tx. We can either make a new tx, or make channel_team_close more complicated. $200


We should use a CLI program to talk to the node instead of using erlang directly.

We need to update download_blocks so that peers get ranked, and we spend more time talking to higher-ranked peers.

download_blocks could be more efficient.

maybe nodes need to advertise their own IP/port combo as a peer?

It would be nice if there were some macros for chalang/src/compiler_lisp2.erl that did backtracking. that way we wouldn't have to think about control flow when making smart contracts.

Updates for next time we restart at a genesis block:


each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible. This stops transactions from being reused on multiple forks.


Make sure that if something was garbage collected from a merkel tree, and we try accessing the thing, it gives a different message than trying to access something that doesn't exist. Make sure we don't assume a block is invalid just because we don't have the proof of it's validity. $200


The current market design charges a 1/10000 fee on every trade. This is to protect from rounding errors.
There is a more elegant way to stop rounding errors.
Set a certain maximum trade size. All orders must be measured in increments of the same size
A limitation of channels is that their output amounts are measured in integers from 0 to 10000.
Every 1 in G of the possible 10000 outputs can be valid.
A1 = amount of money getting matched from our bet,
A2 = amount of money in biggest possible bet,
B = A2 div 10000,
0 == A1 rem B
Making A1 rem B == 0 limits the possible output values of the contract, which slightly reduces liquidity. Being able to reduce the fee to zero is worth this small cost.

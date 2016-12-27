Stuff:

We need to remember the highest-nonced transaction that we signed for our peer. previously called "channel_partner.erl"
We need to remember the highest-nonced transaction that our peer signed for us. previously called "channel_manager.erl" And the highest-nonced scriptpubkey that goes along with it.
we need to remember pairs of channels that are linked by hashlocks. previously called "arbitrage.erl"

We can't let any of this state to crash, so the gen_server should do the minimum computation possible.
We also don't want to update our channel state in parallel, because an attacker could update the same state channel multiple times at once to steal our money.
We need every update to include a before, and after state. That way we wont ever accept contradictory updates.
So we need another gen_server to update the channels one at a time. previously called "channel_manager_feeder.erl"

gen_servers:
channel_manager, arbitrage, channel_feeder

channel_manager:
read/1, delete/1, store/2

arbitrage:
add{bet_hash, cid1, cid2}, del{bet_hash, cid1, cid2}, read/1{bet_hash}

channel_feeder:
make{Tx}, grow{Tx} current{cid}, spend{spk}, close{cid, script_sig}, lock-spend/1{spk}, bet/1{spk}
%before doing anything to a channel, we need to double-check that the channel exists on the top chain. It is possible a fork could happen, and the channel manager is storing a channel that doesn't exist on the chain.

%This would be a lot more efficient if we only recorded diffs and signatures, instead of passing the entire spk every time.

Low-level actions:

1) request the minimum channel balance ratio supported by your partner.
2) make a new channel or grow an existing channel. The amount of money you put into the channel divided by how much they put in must be greater than their minimum channel balance ratio.
3) ask partner for a recent copy of the channel_state.
4) give your channel-partner money.
5) close a channel together.
6) make/modify a bet to give your partner >=0 value, and optionally have them forward the bet to someone else %locked payment
7) Show a scriptsig to your partner to prove that you can close the channel at a particular state. Then you and your partner cooperate to simplify the channel state. Your partner will use arbitrage to close any linked channels at the same height.
8) ask for the list of bets that this node would participate in.
9) make any of the bets from list (8).

high-level actions (built on top of low-level actions above):

* lightning payments. 
* bets by lightning.
* moving a bet to a shorter path. 
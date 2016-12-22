Stuff:

We need to remember the highest-nonced transaction that we signed for our peer. previously called "channel_partner.erl"
We need to remember the highest-nonced transaction that our peer signed for us. previously called "channel_manager.erl" And the highest-nonced scriptpubkey that goes along with it.
we need to remember pairs of channels that are linked by hashlocks. previously called "arbitrage.erl"

We can't let any of this state crash, so the gen_server should do the minimum computation possible.
We also don't want to update our channel state in parallel, because an attacker could update the same state channel multiple times at once to steal our money.
So we need another gen_server to update the channels one at a time. previously called "channel_manager_feeder.erl"

Low-level actions:

1) request the minimum channel balance ratio supported by your partner.
2) make a new channel or grow an existing channel. The amount of money you put into the channel divided by how much they put in must be greater than their minimum channel balance ratio.
3) ask partner for a recent copy of the channel_state.
4) give your channel-partner money.
5) close a channel together.
6) make/modify a bet to give your partner >=0 value, and optionally have them forward the bet to someone else %locked payment
7) Show a scriptsig to your partner to prove that you can close the channel at a particular state. Then you and your partner cooperate to simplify the channel state.

high-level actions (built on top of low-level actions above):

* lightning payments. 
* bets by lightning.
* moving a bet to a shorter path. 
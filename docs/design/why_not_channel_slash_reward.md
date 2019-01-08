I wonder if we can be clever and make channels less interactive, particularly making sure nobody successfully slashes you even if you and your keys get hit offline for the entire time
like: option to give someone who successfully defends you (any public key that is different from yours) a portion of the winnings
so the market host, who has your signed messages, can save you and get compensated
Or some other party

Zack
Third parties can do the slash tx yes.
Mr
but there isn’t an incentive for them to do it right now right?

Zack
We could make a server that accepts small payments and slash txs. If a channel can be slashed, it automatically publishes the tx.
I think not many people want this yet? Hard to know
Mr
idk, it might be nice to completely remove dealing with Channel contesting from the UX

Zack
I don't understand
Mr
if I have a channel open with you, I need to be paying attention in case you decide to try to take all my money

Zack
right
Mr
if I am offline for long enough, you can do this and there is nothing I can do

Zack
so we can make some server that accept small payments and slash tx. if the channel is solo-closed, they automatically publish the tx
That is about as simple a solution I can think of
Mr
right, but that would just be altruism

Zack
how so?
Mr
the publisher of the slash tx doesn’t get compensated for defending you

Zack
if a server doesn't slash when it is able to, then no one would hire it
you can also hire like 5 of these servers run by different people, and you are free to test them and see if they work.
Mr
hiring it would require opening another channel

Zack
the server doesn't know what is a test and what is real
you don't need another channel
Mr
if you make it so that the reward can be recovered on-chain, you don’t need to trust any kind of hiring arrangement
you just need to publicize the channel state, and anyone can race to defend you

Zack
it is a trustfree hiring relationship that doesn't require a channel.
oh, you want the slash tx to have a reward
for the person who publishes it
Mr
right
the reward can come out of the liar’s locked up money

Zack
how do we know which participant is the liar?
Mr
whoever loses the contest

Zack
what if we are currently on nonce 1000, and it is possible to do a slash for 1001 or 1002 or .... or 1010.
Then it is possible to do 10 slash txs on-chain.
How do we know who loses?
Mr
if I try to solo close at a state that isn’t the highest nonce I lose my money

Zack
So whoever did the solo-close is the loser, even if there are more than one slash tx?
Mr
well, if they can successfully defend a slash then they are no longer the loser (the solo-closer)

Zack
the slash txs are made by untrusted third parties. We don't know who they are representing when they publish them.

A troll could publish a slash tx.
a troll could publish 100 slash txs for the same channel
one after another, each updating the nonce a bit more
all in one block even
ideally if a troll does this, it should be expensive for them, and not expensive for the channel participants
Mr
well, they won’t make money because they don’t have the highest nonce channel state at the end of the day
right?

Zack
so we keep a record of whoever made the very last channel slash, and when the channel closes we give them a reward
Mr
yeah. channel slashing is *only* used in disputes right?

Zack
And this reward would be an extra safety deposit charge when forming the channel?
Mr
it would come out of the liar’s locked up money, if there is any left. if not, it comes out of the true side

Zack
channel_team_close saves tx fees. channel slash happens if someone disappears or refuses to cooperate.
Mr
this would be an optional additional channel tx type , not modifying the existing one

Zack
I think we can't tell who the liar is.
I don't see how adding a new tx type would help with the goal you are describing
Mr
not everyone would want to give up some of their reward for a third party to defend them
in what situation do you not know who the liar is?

Zack
If we added the condition that any channel participant who does a channel-solo-close, and their nonce isn't the highest possible, that they should get punished.
This would make a whole class of smart contracts impossible.
We would need that both participants be able to calculate the highest nonce at all times, and that it is impossible a higher nonce will be found.
Mr
if their nonce isn’t the highest possible, but the result of the smart contract execution is the exact same money distribution as a higher nonce execution, then I think there is no difference. if not, then they are liars anyways

Zack
Mr Flintstone
not everyone would want to give up some of their reward for a third party to defend them
if the untrusted third parties are in free market competition, then it doesn't matter whether we pay them for every channel or if we only pay them when they actually close a channel.
The cost of operation is the same, so the average cost of the service will be the same.
in a free market the price of a service approaches the price of providing the service
oh, but we can push all the cost onto whichever participant made a mistake or lied
Mr
right

Zack
the amount of money that goes to each participant is an output of the chalang contract
how do we know how much money to give to the slasher?
the chalang contract would need to be programmed to keep some money in reserve to pay them. otherwise if the liar has 0 veo left, then there is no money to pay a reward
or maybe we need a safety deposit that is different from the smart contract
Mr
Zack
the chalang contract would need to be programmed to keep some money in reserve to pay them. otherwise if the liar has 0 veo left
if liar has zero left it takes from the winner of it all
the winner shouldn’t be too mad, they nearly doubled their money

Zack
when a new channel is made, they could set a safety deposit number. and both participants lock up this amount.
When the channel_timeout tx happens, we refund the safety deposit of whoever was honest, and use the other to pay whoever did the very last channel_slash tx.

But if the channel_solo_close is the final tx, what do we do with the liar's safety deposit?
Mr
then there isn’t a liar imo and it is a normal channel relationship, in your last case

Zack
Mr Flintstone
the winner shouldn’t be too mad, they nearly doubled their money
channels don't necessarily start at 50-50. Could start at 100-0.
Mr
good point

Zack
Mr Flintstone
then there isn’t a liar imo and it is a normal channel relationship, in your last case
so do we give the liars deposit to the other participant? or maybe we just delete it?
Mr
given to the pubkey of the person who published the final slash
but I feel like additional safety deposits are kind of cumbersome
seems elegant to pay out of already locked up money

Zack
what if the amount of money in the channel is less than the reward for slashing?

Zack
I added this to the top of the list of hard fork ideass.
It is only possible to do a channel_slash if the channel participants have shared the spk with you.

It is impossible to tell what spk is currently inside of a channel.
So we would still need the channel participants to pay some small fee to the server that holds their spk.
Otherwise the server would have sybil attacks from billions of junk spks all being stored at once.
so your strategy doesn't get rid of the inconvenience of paying these fees, it only makes the fees smaller.
I wonder how much smaller the fee is?
I guess we need to consider the ratio of the cost to the server of holding the spk, vs making and publishing the tx.

big fee = small_fee * (price of publishing 1000 bytes + price of storing 1000 bytes) / (price of storing 1000 bytes)
Mr
what is a spk?

Zack
spk is the channel state that both parties sign.
ScriptPubKey, like in a bitcoin tx.

1 gigabitpersecond for 1 month is like $70
$70/(24*60*60*billion)

something like $0.0008 to send 1 gigabit.

google drive is like $0.02 to store 1 gigabyte for 1 month.
Mr
ahh, got it

Zack
a byte is 8 bits, so 1 gigabit of storage for a month is like $0.0025
8/25 is about 1/3rd
So it looks like your strategy would reduce the cost of paying 3rd parties to secure channely by about 1/3rd
but it is still less than 1 penny per month per channel
I think at this point, this hard fork just isn't worth the effort to implement
Mr
I am still confused as to why we would need a central server to hold the spks

Zack
1 penny is a lot less than a tx fee, so it isn't meaningful
to do a channel slash you need to provide evidence that closes the channel at a higher nonce than it is currently being closed.
Mr
even if you published them to twitter, the person to publish the spk to the blockchain gets compensated

Zack
the evidence includes an spk and a ScriptSig, which is more evidence that isn't signed by either participant
Mr
ahh

Zack
we could store all the spks as twitter comments, but that is vulnerable to sybil attacks.
Someone could publish billions of meaningless spks.
Mr
but it’s easy to know if a spk is meaningful

Zack
no it isn't.
how would you tell?
Mr
Isn’t the channel referenced in the signed state? Also pub keys

Zack
the channel state is secret. I could have a channel on nonce 1 million, and only show you spks that resolve to nonces less than 1000.
I could show you billions of spks that have equal probability of unlocking for nonce 1000, but I only reveal an ss that unlocks one of the billions at random.
the on-chain channel datastructure just has a channel ID, a quantity of veo locked in it, and a couple of account pubkeys.
you can make any off-chain smart contract without having to change any on-chain state.
That way it is possible to update the off-chain contract instantly.

It didn't work out this time, but don't give up hope. Thinking about stuff like this will help make Amoveo better. Like when you realized oracle fees could be lower for question oracles.
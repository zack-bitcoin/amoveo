Channels are a relationship between 2 participants.
Channels allow for the exchange of value dependent upon the outcome of turing complete contracts.
Channel contracts can be updated without recording anything to the blockchain.

Channels can be used to make contracts with people who you don't have a channel with.
A contract can trustlessly flow through multiple channels, sending value to anyone inside the same channel network.
These contracts can be moved from one channel path to another, trust-free. So less money is locked up enforcing contracts.


Problem scenario:
I received several payments, but my partner closed the channel at the state from before I had received the payments. My channel partner stole my money.

Solution:
Each channel has a `delay` programmed in it.
If your partner tries to close the channel, you have at least this much time to provide data to the blockchain to make the channel close at the correct final state.
Make sure that the `delay` you and your partner agree to is set big enough so that you have time to stop them from stealing from you.
Additionally, you can hire an untrusted third party to provide evidence to the blockchain and stop your partner from stealing from you. You can pay them in a trust-free way by making another channel.
Use a channel_slash transaction.


Problem scenario:
My partner disappeared, but I want to close the channel.

Solution:
You can close the channel and get your money without your partner's help.
Use a channel_solo_close transaction.
You will have to wait at least `delay`, a value you and your partner had agreed upon. 


Problem scenario:
I made a bet in my channel that wont settle for a long time, and my partner wont cooperate for me to make any more bets, and he wont cooperate for me to withdraw any money.
My money is trapped in the channel.
Maybe I have some other short term bets that settle, and the money from them is trapped too.

Solution:
Have a different channel for short term contracts and for long term contracts.
Don't put money into the long term channel until the channel state is already signed.
Remember: all the money in the channel can be trapped for as long as the longest delayed bet in the channel state with the highest nonce.


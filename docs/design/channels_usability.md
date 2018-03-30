
Pan asks:

I don't think it as an attack and support the comment:
''FYI this wasn't a hacker, but a user on the LND slack who had a corrupted channel database, restored an old backup, then closed his channels. Because the backup was out of date, his node broadcast old channel states and his channel partners' nodes detected this as fraud and published the penalty transactions.''

So, if I want run the channel by myself, I should carefully backup the state to make sure it's the last state. Otherwise, I have to lose something and be treated as hacker?

It's quite hard for common people to make a fault tolerant system. So, It's not useable and safe for most people.
I really don't want things progress this way.





Yes this is an important problem. I am glad you bring it up. I have spent a lot of time thinking about this.

You can try out the lightning network in a small testnet on your device using make multi-quick
That way you can test out this error.

But I can tell you it does not happen for us.
The Amoveo light nodes do not ever make a tx. Instead they send the info about the tx to a full node, the full node makes the tx, and then the light node verifies that all the information is correct.

This way it is impossible to close the channel at an old state and get punished. The full node will refuse the generate a tx like this.

It is true that forgetting to save the recent state is a security problem. For now I programmed it so you can download a copy of the most recent state from the full node, if you should lose yours.
This is even sort of trust-free.

The attack we are worried about is if the full node could be giving you an old expired channel state where you have money.
the default software I programmed is honest, and you are free to test the server at any time.
Just request a download from the server, and see if it matches your latest saved channel state.

The server can't tell if you are testing it, or if you actually lost your state channel data. So the server is incentivized to be honest.

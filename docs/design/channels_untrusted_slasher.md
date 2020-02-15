WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//design/channels_untrusted_slasher.md)

One desired property of channels is that users don't want to have to stay online 24/7 to be ready to provide evidence. They would prefer to pay other people to stay online with the evidence.

This creates several new attack vectors we will need to consider while designing the channels.
The old channels looked like this:
new_channel_tx, grow_channel, grow_channel, channel_team_close.

the grow_channels have to each increase the amount of money controlled by the channel.

or like this:
new_channel_tx, grow_channel, channel_solo_close, channel_slash
or like this:
new_channel, channel_solo_close, channel_timeout	

The new channels look like this:
new_channel_tx, grow_channel, grow_channel, channel_solo_close, channel_slash, channel_slash, channel_slash, channel_timeout


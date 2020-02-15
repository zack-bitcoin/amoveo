WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//research_suggestions/secure_light_node_download.md)

The problem is how to securely download a copy of the light node software so that you can trust that the Merkel proofs are valid.

potential solution-

The full node uses it's pubkey in the url, and it signs the light node software it sends to you.
If the signature is invalid for the pubkey in the url, then you don't use it.

The hub doesn't know if you are syncing fresh, or if you already have a copy of the light node software.

The hub makes a fraud proof, so it will lose a ton of money to anyone who can show it signed a bad copy of the light node software.

Many people would be constantly testing hubs, because if the hub lies, they can take all the hub's money.
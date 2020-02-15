WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//design/hedging.md)

Hedging strategies
======


All hedging strategies involve some rebalancing, but we would prefer strategies that need less rebalancing.

Is it ever advantageous to make a derivative priced in the shares from a different derivative?


Imagine Bob wants to bet on a football game, and he doesn't want to be exposed to any Veo risk.

100% veo risk profile: 0*F + 1*V + 0*U.
betting Veo on a football game: 1*V*F + 0*U.
betting USD on a football game: 0*V + 1*U*F. This is Bob's goal risk profile


bet veo in a derivative to go long USD at 2:1 leverage: U*1 - V/2
bet veo in a derivative on the football game at 2:1 leverage: F*V*1 - V/2

Combine both above with the 100% veo risk profile: 
# State channel without off-chain markets

Atomic swaps as invented by TierNolan are very important theoretically. https://en.bitcoin.it/wiki/Atomic_cross-chain_trading
They prove that it is possible to trustlessly exchange currency on one blockchain for currency on another.
You can use a similar mechanism to show that exchanging different currencies on the same blockchain can be trustless.

Unfortunately, atomic swaps have been worthless practically.
It only solves half the problem. We don't _just_ want to swap tokens. We want to swap them at the current market price.
So we also need a decentralized market for matching trades at a market price. The market and the atomic swap need to be trustlessly linked.

Some projects ignore this problem. They don't have off-chain markets, but they put contracts or subcurrencies into channels regardless: Raiden's "instant token swap". Truthcoin's channels. Gnosis' channels. Aeternity's proposed state channels. Augur's use of Ox.

If you are going to trust a centralized market to give your channel the right amount of the right subcurrency/contract, then there is no longer any reason to use channels. Centralized markets are already scalable. The channel is worthless in this situation.

Most state channel projects today do not put any effort into off-chain markets, because for most projects "state channel" is just a buzz word. 

Amoveo is being developed by the person who invented state channels and turing complete state channels. Amoveo cares about doing channels right.

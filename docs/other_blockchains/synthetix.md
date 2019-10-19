Synthetix Review
========

[Synthetix litepaper](https://www.synthetix.io/uploads/synthetix_litepaper.pdf)

Synthetix is a smart contract on the Ethereum blockchain.
SNX is the cryptocurrency that powers Synthetix.
The goal of Synthetix is to allow users to trade synthetic assets, all priced in SNX.

"financial derivative" just means a limited kind of smart contract that works like a bet. 2 people lock money in, and on the expiration date the contract will distribute all the money to those 2 people.

"Synthetic assets" means using financial derivatives to create stablecoins.
In the cryptocurency community, we often use the name "stablecoin" when talking about synthetic assets.
For example, you can use a synthetic USD smart contract to make USD stablecoin.

SNX limiting the volume of stablecoins
===========

Synthetix only supports using the subcurrency SNX as collaterol.
Synthetix targets a collaterollization ratio of 7.5x or more collaterol for all the stablecoin contracts created.

This means that the total market cap of stablecoins enabled by synthetix will be less than about 2/15ths of the market cap of SNX.

They should have just used Eth for this collaterol. There is no reason to have a new subcurrency.

A smaller currency has more volatility, so the interest rate is higher, so it is more expensive to pay someone to lock it up in a contract.
synthetix currently has a market cap 200x smaller than Eth, so the interest rate you need to pay someone to lock up SNX is approximately Sqrt(200) = 14.4x higher than you would need to pay someone to lock up the same value of Eth.

SNX enabling parasite contracts
===========

Synthetix requires holders of SNX to be able to receive fees for staking their SNX.

A parasite contract could re-use oracle and price data provided by synthetix to enable ethereum users to trade stablecoin with all the security of Synthetix, and without having to pay any fee or buy any of the stablecoins produced by the Synthetix contracts.

Miners Front-running
===========

Miners are free to choose the order in which they include the txs in their blocks.
Synthetix has on-chain exchanges for each asset they support. Miners can re-order the txs in these exchanges to profit.

The oracle
===========

The current version of Synthetix has a centralized price feed controlled by the Synthetix team. This means that the Synthetix team can manipulate the price to extract a hidden tax, and we have no way to know how much value they are syphoning from users.
They Synthetix team could publish a wildly incorrect price in the trusted price feed, and in this way steal a large portion of the subcurrency or SNX in one of the Synthetix markets.

When a centralized and trustful tool shoe-horns a smart contract into their interfaces, this seems like a kind of fraud to me.
They are creating the impression that their product is more secure than centralized alternatives.
Their slogan is "Decentralised synthetic assets", but the security guarantees of their product is currently completely centralized and trustful.

The paper mentions possibly using [chainlink as a decentralized oracle in the future. Chainlink is not secure.](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/chainlink.md)

The centralized nature of Synthetix oracle design [has already cause a failure](https://blog.synthetix.io/response-to-oracle-incident/)

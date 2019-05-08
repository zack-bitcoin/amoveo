dxdy review
========

dxdy is a smart contract on Ethereum. https://github.com/dydxprotocol/solo

Anyone can make an account inside the dxdy solo smart contract.
Solo supports multiple subcurrencies. You can deposit any supported subcurrency into your account inside the solo smart contract.
You can lock up any of the currencies in your account as collateral in order to borrow any other kind of subcurrency.
Eventually, either the margins of your contract are exceeded, which causes your collateral to be confiscated by solo, or you re-deposit the coins you borrowed plus interest in order to get your collateral back.

The interest you have to pay to unlock your collateral can be positive or negative. It attempts to react to market forces by looking at how other people are using solo.

problems with this design
=========
* on-chain markets will get front-run by miners.
* the on-chain mechanism to calculate the interest rate will always be delayed from the true relative prices of the subcurrencies.
* the on-chain mechanism to calculate the interest rate could be manipulated to drain value from Solo users.
* doing derivatives on-chain is slow, expensive, and it is not scalable. These contracts should be inside the lightning network.
* agreeing to a contract without knowing the interest rate for that contract is a radical departure from how existing derivatives work. Why would people make trades in a market where they cannot even choose the price they will trade at?

on-chain market gets front-run by miners
========

Miners are free to choose the order of the txs that they include in every block, so they will put the txs in an order that allows them to extract value from dxdy markets.
Here is a research paper exploring this failure mode in depth https://arxiv.org/abs/1904.05234

interest rate calculation delays
=======

Cryptocurrencies are volatile.
Imagine a scenario where one of the subcurrencies supported by the Solo smart contract suddenly had its real off-chain interest rate change radically.
Then there will be arbitrage opportunities between Solo smart contract trades vs other derivatives mechanisms which can react to changes in price instantly.
Anyone invested in the solo smart contract will be bleeding money to the arbiters until the dxdy price adjusts to where it belongs.

interest rate manipulations
======

If we adjust the interest rate calculation algorithm so that the interest rate can change more rapidly, that opens us up to this other vulnerability.
An attacker can trade with themselves in the solo smart contract to manipulate the interest rate calculated by the solo smart contract.
He can manipulate the interest rate to profit.

If the interest rate calculation is too slow, the solo smart contract fails because of arbitrage. If the interest rate calculation is too fast, it fails because of interest rate manipulations. If the interest rate calculation is medium, then it fails both ways at once.

on-chain derivatives are not scalable
========

trading in the solo smart contract is all on-chain in ethereum. You need to publish transactions inside of ethereum blocks in order to participate in any way.

on-chain transactions are slow. You have to wait for the tx to be included and for confirmations.
on-chain txs are expensive. You have to pay the miner a gas fee to include the tx.
on-chain txs lack privacy. Everyone can see what you are doing.

It is best to have derivatives inside of state channels to avoid all these short comings. If dxdy costs more than 10x as much as competitors, this will cause dxdy to fail.

Contracts with mystery interest rates
========

Derivatives are a hugely popular class of financial contracts that have been in use since before written history.
Thousands of years of evolution by trial and error have resulted in a beautiful design that is replicated in all popular derivatives markets today.

The purpose of a derivative is for allowing a pair of people to trade risk.
For a derivative to be useful, we need to be able to measure the quantity of risk being exchanged.
Only by accurately quantifying the risk being traded can we calculate the price being paid for that risk.

Having an unknown additional risk of interest rate fees is contradictory with the goal and design of derivatives. It makes it impossible to calculate the price you want to trade at, because there are unmeasurable risks mixed into the contract.

The prices of contracts in Solo don't tell you about the true prices of these assets, because the additional risk of the interest rate fees skews everything in an unknowable way.

> A Channel server host is essentially a market maker?

 A channel server can be market maker. It is also possible to have a channel server that only processes lightning payments, and has no markets. There could be a channel server that is just for playing black jack the gambling card game.

> What is the ease/difficulty of programming such a server?

The software to run a channel server is included with the amoveo source code. Running a server is this simple: rent a server, install amoveo, and send money to the server's wallet.

> Amoveo markets are mostly binary options?

Amoveo channels are programmable, so any type of market could exist. Currently, I have only programmed a market for binary options. Eventually, I will make more markets.

> Are the block times equal to the batch periods in Amoveo for on chain markets?

The batch period for a market is something you choose when making a market on your channel server. It is measured in blocks, and the shortest possible batch is 1 block of time.

> Why isn't Amoveo an app on Ethereum?

On-chain markets can't scale with off-chain assets. Building off-chain markets for Ethereum channels seemed pretty hackish. Raiden needed a big team, and it is far simpler.

Since the smart contracts are all off chain, Amoveo can be designed differently than Ethereum to be much more scalable.

Amoveo is a lot smaller and simpler than Ethereum. I cut out complexity that isn't necessary for off-chain smart contracts.


> How would one go about getting the transactions of an address?

In order to get the transactions created by an address, you would have to look at every block and check if that address made an payment. This is not easy to do by default.
`PBlock = block:get_by_height(4).` would look up the 4th block_plus and store it in a variable.
`block:txs(block:block(PBlock)).` returns the transactions spent in that block.
Each transaction type has a different command for checking who spent that transaction. You can look up commands to deal with transactions in /src/consensus/txs/


> How will you get people to work on Amoveo?

Amoveo is a new way for people to work together. Amoveo can bootstrap itself. Amoveo will build its own team and hire its own employees without anyone's input. We will use [insured crowdfunding](use-cases-and-ideas/insured_crowdfund.md)

> How large is your potential user base?

The potential user base is huge. Online gambling for example is a massive market. Amoveo will be trustless and have lower fees. This is just one of the markets that Amoveo will disrupt.
The market for financial derivatives is the biggest market in the world.

> What’s your plan to market what you create?

We can use Amoveo smart contracts to incentivize people to advertize for Amoveo.




> If I understand correctly, everything is valued in Veo and you are not tokenizing real world securities. I do not think this is in direct competition to the financial derivatives market. Most derivatives are used for hedging, if everything becomes priced in Veo it adds an extra dimension because now the volatility of Veo has to be factored in. It also means some option sellers will have extra capital costs because now they have to own Veo and their shares, instead of just their shares. If I got that right, Amoveo will be competing for markets like sports betting and perhaps other types of speculation/prediction.


I think you are confusing financial derivatives with things like stocks. Stocks allow you to tokenize ownership of something. A derivative is a financial asset that gets its value from some publicly available information.

The total market cap of stocks world wide is around $80 trillion. The total market cap of derivatives is around $1200 trillion. People are not so interested in tokenizing. People are 100x more interested in betting.

A cash settled derivative is economically the same as a derivative for the actual asset.

Using traditional financial tools, if someone is owning stock and a financial derivative, it is usually stock and a put option. This way there is a limit to their loss. This financial strategy has the same capital costs using traditional finance or using a blockchain.

Another common strategy is a bond and call option. Together, they can give the same risk profile as a stock and a put. This financial strategy has the same capital costs whether using traditional finance or using a blockchain.

> Could you give a specific example of a financial strategy that would require locking up more capital on the blockchain in comparison to traditional finance?

Financial derivatives allow you to hedge risk. So long as there are people who want to invest in Amoveo with leverage, then it will be affordable to make stable-coins. I expect most financial derivatives to be priced in stable-coins. Amoveo is a Turing complete platform, so you can program stuff like this.

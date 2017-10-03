* Why isn't Amoveo an app on Ethereum?

On-chain markets can't scale with off-chain assets. Building off-chain markets for ethereum channels seemed pretty hackish. Raiden needed a big team, and it is far simpler.

Since the smart contracts are all off chain, amoveo can be designed differently than ethereum to be much more scalable.

Amoveo is a lot smaller and simpler than ethereum. I cut out complexity that isn't necessary for off-chain smart contracts.


* how would one go about getting the transactions of an address

To get the transactions created by an address you would have to look at every block, and check if that address made an payment. This is not easy to do by default.
`PBlock = block:get_by_height(4).` would look up the 4th block_plus and store it in a variable.
`block:txs(block:block(PBlock)).` returns the transactions spent in that block.
Each transaction type has a different command for checking who spent that transaction. you can look up commands to deal with transactions in /src/consensus/txs/

* if you are running a node and you query the balance of another address?

To query the balance of another address is difficult. You would need to look up every account until you found the account that stored the matching address.

Querying the balance of an id is easy. do this: `accounts:balance(api:account(Pubkey)).`


u have
ids
accounts
addresses

[9:52]
and i cant figure out the functionality of each.

zack
[9:52 PM]
ids are the integers for looking up accounts. when you send money to someone, you need to know their id.

[9:52]
an account is the part of the on-chain state that records the address and balance of one of the users.

[9:53]
an address is the hash of a pubkey.


> How will you get people to work on Amoveo?

Amoveo is a new way for people to work together. Amoveo can bootstrap itself. Amoveo can build its own team and hire its own employees without anyone's input.

> How large is your potential user base?

The potential user base is huge. Just online gambling for example is a massive market. Amoveo will be trustless and have lower fees. This is just one of the markets that amoveo can distrupt.

> What’s your plan to market what you create?

Amoveo can hire people to advertise for Amoveo. We don't need a plan.



> If I understand correctly, everything is valued in Veo and you are not tokenizing real world securities. I do not think this is in direct competition to the financial derivatives market. Most derivatives are used for hedging, if everything becomes priced in Veo it adds an extra dimension because now the volatility of Veo has to be factored in. It also means some option sellers will have extra capital costs because now they have to own Veo and their shares, instead of just their shares. If I got that right, Amoveo will be competing for markets like sports betting and perhaps other types of speculation/prediction.


I think you are confusing financial derivatives with things like stocks. Stocks allow you to tokenize ownership of something. A derivative is a financial asset that gets it's value from some publicly available information.

The total market cap of stocks world wide, is around $80 trillion. The total market cap of derivatives is around $1200 trillion. People are not so interested in tokenizing. People are 100x more interested in betting.

A cash settled derivative is economically the same as a derivative for the actual asset.

Using traditional financial tools, if someone is owning stock and a financial derivative, it is usually stock and a put option. This way there is a limit to their loss. This financial strategy has the same capital costs using traditional finance, or using a blockchain.

Another common strategy is a bond and call option. Together they can give the same risk profile as a stock and a put. This financial strategy has the same capital costs whether using traditional finance, or using a blockchain.

Could you give a specific example of a financial strategy that would require locking up more capital on the blockchain in comparison to traditional finance?

Financial derivatives allow you to hedge risk. So long as there are people who want to invest in Amoveo with leverage, then it will be affordable to make stablecoins. I expect most financial derivatives to be priced in stable-coins. Amoveo is a turing complete platform, so you can program stuff like this.

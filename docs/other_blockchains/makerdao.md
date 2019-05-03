MakerDAO
==========

MakerDAO is a large stablecoin project.
It is used to create a subcurrency called dai on ethereum.
It is designed with the goal that dai should have a relatively constant value.

The goal of this document is to explain why MakerDAO will fail, and to try and predict how it will happen, and how much of the value will get destroyed.


MakerDAO has 4 subcurrencies:
* dai - stable value
* cdp - long-eth
* peth
* mkr - governance tokens

eth can get spit into dai and cdp. dai and cdp can be combined to reclaim the eth.

The net risk from combining dai and cdp must be the same as the eth they were made from. Since dai is stable, that means cdp must be long-eth.
But it is an unusual way of formating long-eth. The amount of eth the contract pays out is constant, but the cost of the dai measured in eth, to unlock the eth from the cdp is a variable.

The holders of mkr vote to decide on a team of trusted feeds, they call it an "oracle". The trusted feeds are used so the blockchain can know the current exchange rate between eth and USD. This exchange rate is used to determine the price at which people can exchange dai for cdp.

It is enforced in a weird way.
Both dai and cdp are spendable, but only dai is fungible.
Every cdp contract can have a different exchange rate to pay dai and get the eth out, it depends on the price in the oracle at the time the contract was made.

If the oracle says the price of eth is higher, that means it costs less to produce the same amount of dai. So everyone will be incentivized to combine dai with their old cdps to get the eth out, and then make new dai at the new better price. If Eth goes up 5%, then cdps increased in value 10%.

If the oracle says the price of eth is 5% lower, then cdps drop in value by 10%.

The order of events of the attack:
1) attacker buys cdp contracts, selling all the dai at the current high price. (spend $100 of eth, get $100 of cdp)
2) the oracle lies to say Eth is more valuable than it actually is.
3) attacker buys (or makes) dai at the new lower price, and uses it to unlock his eth from step (1). (spend $50 of eth, get $100 of eth)

net effect: attacker is $50 richer in eth, there is $50 of eth locked in a cdp that would cost $100 of dai to unlock. The system is now less collatoralized.

If CDP contracts are sufficiently under-collateralized, this could cause automatic liquidation of the cdp.
The makerDAO documentation is complicated past this point, it is not clear to me if this will result in hyperinflation of peth, or if it will cause fees to increase until it isn't worth it to use dai.


All that is left is to prove that we can get the oracle to lie at a cheap price.
voting does not work because it is an inherently unstable game https://vitalik.ca/general/2019/04/03/collusion.html
It is vulnerable to bribery because of tragedy of the commons https://github.com/zack-bitcoin/amoveo/blob/master/docs/use-cases-and-ideas/tragedy_of_commons_in_voting.md
It is mathematically impossible to give it the properties we want https://en.wikipedia.org/wiki/Arrow%27s_impossibility_theorem
An attacker can even bribe a vote to force the outcome, and they don't have to pay the bribe https://blog.ethereum.org/2015/01/28/p-epsilon-attack/

Makerdao lets the holders of mkr vote to determine which trusted feeds will participate in the oracle.
So it is possible to corrupt the mkr holders to have yourself voted as oracle over and over until you control all the oracles.

Merging the data from many trusted feeds is also a kind of voting protocol. So you can also manipulate the outcome of the oracle by bribing the trusted feeds who were already elected.

mkr and dai together are worth over $800 million, so when this theft occurs, it could cause a lot of damage.

The margins for dai are 150%, so you can make the oracle lie by up to 150% in each direction, and you can earn %150 of your investment for doing this attack once.
But if you did this attack by bribing the oracle directly, the mkr holders would probably do a quick vote to kick all the bad price feeds out.
So in this case there would be rare attacks that steal a lot of money.

If the attacker instead bribed the mkr holders, then they can do this attack over and over indefinitely. In that case it might make sense to only steal like 2% at a time, maybe people wont notice so that the attacker can steal money for a long time.
Or maybe it is just better to do a big attack all at once instead of stretching it out.

Game theoretically speaking, this is a 4.2 level security mechanism, so it is inevitable that an attack will occur. [you can read about security levels here](../basics/trust_theory.md)


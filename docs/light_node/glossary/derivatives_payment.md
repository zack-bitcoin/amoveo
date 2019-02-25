Derivatives Payment
===========

When you make a derivative from the otc_derivatives page, you need to say how much you are paying or being paid for this contract.

For example, I want to double my VEO exposure quickly.
To convince many people to sell me their VEO exposure right away, I offer to pay them a 5% premium.
So if they buy $100 of stablecoin from me, and hold it for a month, they will have $105 at the end of the month.

To do this, I need to tell the otc_derivatives page to lock up $100 of veo into the stablecoin contract, and to keep to just pay $5 to the other person directly.

The otc_derivatives tool atomically combines the payment with the smart contract, so neither of you has any risk.

If you want to pay someone to do a contract with you, this value should be positive. If you want to get paid to make a contract with someone, then you should make this number negative.
Oracle Question
==========

This is a question that people will read to decide how to report information to the blockchain.

Example binary oracle questions:
"Donald Trump was elected president for a second term."
"The high temperature in New York on March 2, 2019 will be less than 25 Celcius."

Example scalar oracle questions:
"The price of USD in VEO from 0 to 0.05 on March 2, 2019."
"The price of BTC in VEO from 0 to 130 on March 2, 2019."
"The price of BTC in USD from 0 to 10 000 on March 2, 2019."

If you want to make a stablecoin, it is important to always ask for "the price of X in VEO". Do not ask for "the price of VEO in X", or else the contract will not work as a stablecoin.

The otc_derivatives page can auto-fill the upper oracle limit from the oracle text, if you format the oracle correctly.
otc_derivatives searches for the word "from", then it seraches for the word "to", and it assumes that the next number displayed is the upper limit of what the oracle can measure.
So it is good to use the substring "from 0 to ", if you intend to use it for stablecoins.
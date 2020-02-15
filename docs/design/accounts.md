WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//design/accounts.md)

# Accounts

The following describes accounts, which are one of merkel trees, responsible for keeping address and balance associated with it.

In order to create an account one has to pay fee, which prevents spam.

Account deletion is rewarded, which incentivizes the reclaiming of space.

## Overview

Account consists of:
* Pubkey
* Balance - amount of money that is associated with an account
* Nonce - number incremented with every transaction that is put on the chain
* Height - height of account last update
* Bets - pointer to merkle tree that stores how many bets have been made in each oracle




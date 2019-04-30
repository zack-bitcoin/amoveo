Chainlink oracle review
=========

Chainlink is a subcurrency on Eth called LINK, as well as a bunch of tools in javascript and golang.
The goal of chainlink is to make oracles. The oracles will provide data to the blockchain so that we can have contracts that depend on events like the result of a football game, or the price of Euro.

Chainlink's strategy for security is not so good. They seem to think that if they describe a large enough volume of different mechanisms, that no one will be able to find a bug in the tangled mess.

Luckily I have developed a simple to calculate model for trust https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md
We can use this model of trust to quickly find out if Chainlink can be a secure oracle or not.

A key concept we will use to simplify this analysis is that a mechanism can only be as secure as it's weakest part.
As stated in the chainlink white paper: "The security of any system is only as strong as its weakest link"

It is almost always a bad idea to layer different oracle security mechanisms on top of each other, since the resultant mechanism will be as insecure as the least secure of the component mechanisms.



The layers of oracle in-security implemented in Chainlink are:
* api data-source
* data source aggregation
* non-spendable reputation
* spendable reputation
* certifications
* trusted hardware


api data-source
===========

The chainlink team is already aware that trusting a central api provider is not a secure oracle.
From the chainlink white paper "There is, of course, noperfectly trustworthy data source. Data may be benignly or maliciously corrupted due to faulty web sites, cheating service providers, or honest mistakes."

In particular, the cental person running the API could choose to change the data provided over time, or to give different data to different parts of the oracle to cause oracle participants to lose their reputation. In this way, the person who controls the api can not only corrupt data from their own api, they can cause other parts of the oracle system to break down.

Trusting an api data source is a level 4.1 security. 

data source aggregation
=========

When you aggregate data from multiple sources, this is called a voting protocol. Voting protocols are well studied in the context of blockchains.

Vitalik wrote this great paper explaining how aggregating data from multiple sources does not increase the security of that data. https://vitalik.ca/general/2019/04/03/collusion.html

data source aggregation is level 4.1 security.

oracle non-spendable reputation
=======

The oracles in chainlink have a kind of reputation that they cannot spend to each other. Like a record of the total number of assigned requests for their node.

Non-spendable reputation actually decreases the security of the oracle, because it increases the financial incentive to participate in retirement attacks.

If there is no way to sell the reputation, and a person wants to retire from being an oracle, then a retirement attack is the only way that they can transform their reputation-value into a spendable form.

non-spendable reputation is 4.1 level security.

oracle spendable reputation
======

Spendable reputation is the security system used in Augur and Bitcoin Hivemind.
This is the only layer of chainlink that has the potential to provide more security vs a centralized trusted third party.

As long as the value of reputation owned by the oracle is greater than the total number of bets being judged on by the oracle, then this system has level 3.1 level security.

Maintaining the high value of reputation means that people using the oracle data need to pay transaction fees to the oracle.
In the long-run, this is impossible to enforce because people will program parasite contracts that use the oracle data without paying the oracle for the privilege.

Once there are enough parasite contracts, then the 3.1 level security will break down, and the mechanism will be at level 4.1


Certifications
========

Trusting one centralized source of data to tell you if another centralized source of data is trustworthy.
This does not increase the security at all, both centralized sources of data can be lying to you.
4.1 level security.

Trusted hardware
=======

How can a person know if the request they send is being responded to by real trusted hardware, or if it is being responded to by a fake enclave being virtualized?

Real trusted hardware will have a private key embedded in it by the hardware manufacturer, and it is impossible to extract the private key without breaking the hardware.
So you can contact the hardware manufacturer, and download a list of public keys for all the hardware that they have created. You can use these public keys to verify that a signature was indeed created inside of the trusted hardware.

The problem is that we are trusting the hardware manufacturer. They could have saved copies of the private keys in their hardware, allows them to sign any message so to us it will look like the trusted hardware produced that message.
The hardware manufacturer could add more pubkeys to the list, even if those pubkeys do not correspond to any hardware that they have produced. This gives the hardware manufacturer the ability to run an oracle where they can make it lie.

If the hardware manufacturer can break the oracle system, then eventually governments will pass laws to force the hardware manufacturer to break the oracle. They will use excuses like "saving the children", and "preventing criminal terrorism".

Trusted hardware is level 4.1 secure.



Conclusions
========

level 4.2 oracle systems like chainlink are more expensive and less secure than level 3.2 oracles like @AugurProject and @BitcoinHivemind . Which are less secure and more expensive than level 2.2 oracles like Amoveo.
WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//progress_reports/August_2018_summary.md)

In AE/Augur/wagger, there is always some group of people who control the outcome of the oracle. You need to pay very high fees to these people to bribe them to not steal the money at stake in the markets. If they think that the long term profit of fees exceeds the short term profit of stealing the money from the markets, then they wont steal.
By avoiding this inefficiency, Amoveo's oracle can run for orders of magnitude lower cost.
Our question oracles currently cost about 22 mVEO each.

Furthermore, having to pay a group of people to control the outcome means that if anything prevents these people from getting paid, then the oracle will fail. In systems like AE/Augur/wagger, if ever someone finds a way to use the data from the oracle without paying the oracle for this data, then the oracle will break.
This problem is called the "parasite contract problem".
Avoiding this problem on turing complete systems like Ethereum or AE is impossible. People can always write parasite smart contracts on a turing complete system.

Since Amoveo doesn't have any central party we need to bribe to secure the oracle, we do not have the parasite-contract problem.
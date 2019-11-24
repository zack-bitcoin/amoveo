Avalanche review
========

Avalanche is a idea on how to build proof of stake consensus for blockchains.

[review of other papers from the same author](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/Emin_Gun_Sirer.md)

Here is the avalanche white paper: https://avalanchelabs.org/QmT1ry38PAmnhparPUmsUNHDEGHQusBLD6T5XJh4mUUn3v.pdf

How does Avalanche work?
========

The idea of Avalanche consensus, is that if some decision needs to be made, everyone can make a non-committed prediction of what decision will be made, then they can query their peers for their predictions. Everyone changes their own prediction to be whatever was the most common prediction out of everyone they had queried.

Others have reviewed Avalanche
========

[On pseudo-profound bullshit in the Avalanche whitepaper](https://file.globalupload.io/YWV9p77DX7.pdf)

The goal of this paper is to try and make these concepts understandable to a wider audience.

Assuming 100% of users behave honestly, does Avalanche work?
==========

Assuming that 100% of the users of Avalanche participate honestly, this is a kind of voting protocol where each user's influence over the outcome is proportional to how much stake they control in the system.

Voting can never work in blockchains: https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md

So Avalanche is level 4 vulnerable to soft fork bribery attacks, even if 100% of the users follow the rules 100% honestly. So Avalanche is worse than centralized alternatives, even if 100% of users are honest.

How bad is it if some users cheat?
============

Avalanche is actually even worse than normal voting protocols, because the users wont behave 100% honestly. There is no consequence for disobeying the rules about what prediction you display.

For example, if there are 2 txs that are contradictory, and you would benefit from having tx B included instead of tx C, then it could be in your interest to always predict that B will win, even if a majority of the peers that you query choose C.

If no one else is cheating, then cheating this way is a great strategy. If there are 10k nodes with equal stake, and you can get sqrt(10k)=100 of them to cheat, then it is almost certain you will succeed at causing your prefered outcome to win.

Since cheating this way is profitable, we can expect that a majority of users will do it.
But, if >1/3 of stake is cheating to make outcome 1 win, and >1/3rd of stake is cheating to make outcome 2 win, then Avalanche will be unable to add any more blocks. Progress will freeze, and there is no fair rule on how to recover from this state.

So Avalanche is insecure like a voting protocol, but it is even worse because it freezes in unrecoverable states.

More reading on this topic
==========

Here is a paper showing that all PoS blockchains are vulnerable to soft fork bribery attacks https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md
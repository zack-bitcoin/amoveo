There is some concern about how Amoveo survives certain attacks against the oracle.

The situations people worry about is like this:
1) An attacker spends lots of money making bets in the oracle for an outcome that is false.
2) An attacker bribes the developers to update Amoveo so that one of the oracles can only resolve on a lying state.



An attacker spends lots of money making bets in the oracle for an outcome that is false.
========

If no one is betting in the oracle, then maybe no one will notice it closing incorrectly. But since no one has money at risk, it doesn't matter if it closes incorrectly.

If someone is betting in the oracle, then they have an incentive to keep an eye on it and notice if it is closing incorrectly. They will have at least a week's warning before it closes wrong.
They can alert the other users about what is happening.
People running full nodes can instruct their full node to not sync any blocks that close the oracle in the lying state.

For convenience purposes, if it is too annoying to update your full node, you can instead download a version of the full node already set up to sync on the side of the fork you prefer to sync on.

A basic assumption of Amoveo is that the version of history which is more accurate will have coins that are more valuable.

So the attacker will end up winning lots of money on the worthless side of the fork, and everyone else will earn lots of money on the valueable side of the fork.


An attacker bribes the developers to update Amoveo so that one of the oracles can only resolve on a lying state.
========

Just because I want a hard update to happen doesn't mean I can get it to happen. Sometimes the mining pool operators make me set up a testnet first so they can try it out.
I walk mining pool operators through what has changed for every update.
Additionally, I write a report for the community explaining what the update will change and why they should be willing to participate.

If I lied and tricked the community into doing an update that let me steal some money, this would destroy my reputation with them, and it would become vastly more difficult for me to get more updates into Amoveo in the future.
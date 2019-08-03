Parasite Contracts
=======

Many oracle designs require that traders who use the oracle must pay a fee to the oracle reporters.
A "parasite contract" is a tool that allows you to make a bet, and the outcome of the bet is determined by the oracle, and the betters don't pay any trading fee to the oracle reporters.
If a oracle's security is dependent on getting these trading fees for oracle reporters, then that oracle is vulnerable to parasite contracts.

Amoveo is secure against parasite contracts because traders in Amoveo don't have to pay any fee to oracle reporters. A parasite contract attack against Amoveo is identical to the standard way of trading contracts.

How hard to prevent?
======

If your blockchain supports light nodes, then parasite contracts can live on other blockchains and attack the oracle on your blockchain. So even if you prevent parasites on your blockchain, you have not completely stopped this attack.

A soft update on Ethereum to prevent any smart contracts from accessing Augur's data, such an update would almost certainly result in DOS vulnerabilities. Because you can't know if the contract will access Augur's data until you have run the contract.

Parasite contracts are impossible to prevent, they are something that we need to learn to live with.
If an oracle breaks because of parasites, then it is a poorly designed oracle.


Augur's experience with Edmund Edgar
========

Edgar wrote about what he is working on.
https://medium.com/@edmundedgar/realitio-the-crowd-sourced-smart-contract-oracle-now-in-a-real-money-trial-on-mainnet-f46bf016759d
> "we are working on plug-ins to connect to existing trustless coordination systems. This work-in-progress contract uses Augur as its final arbitrator, providing the same security assurances as Augur itself but at a fraction of the cost in the typical case."

I ask him about this, and he shared more about his experiences having written a parasite contract to attack Augur: https://medium.com/@edmundedgar/the-parasite-and-the-whale-7cb3c87e9902

It seems he stopped pursuing the parasite contracts for now, instead using some other oracle besides Augur.

Augur's experience with Veil
======

EDIT:  Since I wrote this, a third party investor in Augur told me a rumor that Veil told the Augur devs that Veil does not have any intention of doing parasite contracts. Apparently Veil has agreed to voluntarily pay money to Augur in proportion to how much they use Augur's oracle.

EDIT2: Veil publicly posted that they are strategically avoiding making any parasite contracts, because they don't want to harm Augur.


On Augur's stack exchange, that they have been unable to find any solution to this problem: https://augur.stackexchange.com/questions/29/how-does-augur-deal-with-the-problem-of-parasitic-oracles

On the Augur wiki https://en.bitcoinwiki.org/wiki/Augur_(software) a mention of parasite contracts:
"One critic has expressed the opinion that the Ethereum platform is impractical for use in the calculation of outcomes because it allows for "parasite contracts" which steal the work done by contributors without compensating them properly. The Augur team pointed out that this vulnerability is not unique to Ethereum: any blockchain-based system can execute such a "parasite" attack on another blockchain simply by incorporating Merkle proofs of the host blockchain into its own chain. Ethereum-based oracles are expected to be resilient to parasite attacks, as they cannot be attacked by Ethereum-based parasite contracts; Ethereum contracts do not have access to raw blockchain data."

The Augur team's expectations have not held true. Veil has become a parasite contract that exists on Ethereum.

On Veil's github, they have released a parasite tool https://github.com/veilco/augur-lite , as they describe it "No oracle is built into the protocol. Instead, markets have a resolver which can reference any oracle—an Augur market, Chainlink feed, or any arbitrary smart contract state."
"Veil uses a smart contract called OracleBridge to observe the result of Augur markets and resolve AugurLite markets accordingly."

Joey Krug commenting on this parasite tool here: https://twitter.com/joeykrug/status/1128732458193752065
You can see his concern with the situation, even if he does not directly explain why this is so bad.


Veil is the death of Augur.
Augur died in May 2019, but it will take a while for the investors to realize what happened.

It is ironic because Veil was supposed to be the UX experience that would bring Augur mainstream, but instead they turned out to be the attackers who destroyed Augur.

Augur's failure brings serious doubts to whether Bitcoin Hivemind could ever work. Bitcoin Hivemind's current design has the same vulnerabilities to parasite contracts as Augur.
Bitcoin Hivemind's advantage is that they don't allow you to program new smart contracts. So you can't build the parasite contract onto the same blockchain.

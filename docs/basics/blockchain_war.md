Blockchain War
==========

August 2019

Amoveo the blockchain is providing resources and support for an aggressive mining pool that is destroying one of the competitor blockchains. This is causing value from that other blockchain to be syphoned into Amoveo. This benefits anyone holding Veo tokens, and anyone who is mining Amoveo.

The competitor blockchain has a less effective retargetting algorithm than Amoveo, and Amoveo takes advantage of this fact to conquor them.

Based on the hashrate in Amoveo, it seems that a mining pool is doing a difficulty oscillation attack against one of the other FPGA mining enabled blockchains. The attacker is mining Amoveo during the recovery phase of the attack.

A difficulty oscillation attack is when a mining pool periodically puts a lot of hashpower into one particular blockchain in an effort to cause the difficult of that blockchain to start to oscillate as well.
The attack is successful if the attacking mining pool manages to mine during the periods of low difficulty, and they don't mine during the periods of high difficulty.

Motivations
========

This attack does not require any malicious intent on the part of the attacking mining pools.
Switching blockchains to mine the more profitable coin is a common strategy for mining pools, and this attack can happen as an unintended side-effect of this common mining strategy.

This attack does not require any malicious intent on the part of the attacking blockchain.
Having the fastest retargetting algorithm is an important defensive measure, the fact that it causes other blockchains to get attacked is an unintended side-effect of having such secure defenses.

Analogy with Light
========

Light is waves in the electromagnetic field that carry energy.
Each light particle follows the path of least action between two electrons.
These waves are in the blockchain difficulties and they carry value.
Each difficulty-wave follows the path of maximum profitability between pairs of blockchains. The miners are trying to extract as much value as they can, and the value they fail to extract overflows into the blockchain that captures the difficulty-wave.

A blockchain that is emiting difficulty-waves is leaking value into it's environment. A blockchain that captures difficulty-waves is absorbing value from it's environment.


Implications for Blockchain Engineering
=========
This could put limits on blockchain design that are constantly shifting, with the moving battle-fields.
Your design can suddenly become insecure, because of what someone else does in a completely different blockchain.
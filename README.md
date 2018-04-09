Amoveo
==========
[Español](docs/es/README.md)

(work in progress) [中文](docs/cn/README.md)


A blockchain for enforcing contracts for investing and insurance. 

Amoveo contracts use state-channel technology. This means fees are low, contracts are nearly instant and can support a global audience.

Amoveo has oracle technology.
Amoveo can learn facts about our world and enforce the outcome of contracts that are governed by these facts.
This, for example, enables you to bet on the price of Amazon shares.

The variables that define how to participate in Amoveo can be modified by the Amoveo community using a built-in process.
This way Amoveo will always stay optimally tuned to produce the best results.


[Amoveo whitepaper](docs/white_paper.md).

Amoveo main net will launch at 11:00 AM GMT on March 2, 2018.

## Community
[Amoveo forum on reddit](https://www.reddit.com/r/Amoveo/)

[Amoveo announcements on twitter](https://twitter.com/zack_bitcoin)

[Statistics page to see historic difficulty, and more](https://jimhsu.github.io/amoveo-stats/)

[Network page to see the nodes that power the network](http://185.117.73.74/amoveo-network-status)


## Light node
Simply visit [this webpage](http://159.65.120.84:8080/wallet.html) to participate in Amoveo, no installation necessary.

You can also download the javascript light node to ensure you have the secure version, and to be able to run in cold storage. open this file with your browser: `apps/amoveo_http/priv/external_web/wallet.html`

This light node downloads headers and verifies the proof of work.
It verifies merkle proof of all blockchain state you download to give as much security as a full node, if you wait enough confirmations.
You can use the light node to participate in [markets](docs/light_node/market.md).
A light node that currently has markets is [here](http://159.89.106.253:8080/wallet.html).
and the interface to look at the markets is [here](http://159.89.106.253:8080/explorer.html).


## Block Explorer
The block explorer for the network is [here](http://159.65.120.84:8080/explorer.html).

Another explorer is [here](http://159.89.106.253:8080/explorer.html). This one has some markets where you can gamble.


## Full node
[Launch an erlang full node and connect to the network](docs/getting-started/turn_it_on.md)
[Issue commands to your full node](docs/api/commands.md)
commands such as:
* turning the node off without corrupting the database.
* looking up information from the blockchain or it's history.
* making a server that collects fees by routing payments or making markets
* participating in the oracle mechanism or governance mechanism.

## Mining
[Here is a miner for Nvidia GPU, it works for linux and windows](https://github.com/Mandelhoff/AmoveoMinerGpuCuda)

[Linux and Mac OSX CPU miner, only for educational purposes](https://github.com/zack-bitcoin/amoveo-c-miner)

It uses SHA256 like bitcoin. But it is a little different, so bitcoin ASICs cannot be used to mine Amoveo.
Full node keys are stored in `_build/prod/rel/amoveo_core/keys/keys.db`

Here is a list of mining pools you can connect your miner to:

http://159.65.120.84:8085/main.html

http://amoveopool2.com/pool

http://amoveo.noncense.tech/


## Software to launch a new mining pool
If your full node is on a different machine from your miner, you will need a mining pool. [Mining pool software can be found here.](https://github.com/zack-bitcoin/amoveo-mining-pool)
The "master" branch is for paying out small amounts to each miner with each block.
The "classic" version only pays a miner when they find a block.


## Developers

If you want to build on top of Amoveo [read the develper's guide](docs/getting-started/quick_start_developer_guide.md)


## Donations

```
Bitcoin Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT
Veo donations: BGH+3P768A9cSNR3GLSRXgsokSL/Jdbm+rOJogbgiPxq8M+J2R4nVxZ+Hj6WdI4rMsq6nPzkMh77WGBCMx89HUM=
Gifts must be less than $10 000 per person per year.
```



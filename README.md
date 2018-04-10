Amoveo
==========
[Español](docs/es/README.md)

(work in progress) [中文](docs/cn/README.md)


Amoveo is a blockchain meant for enforcement of investment and insurance contracts. 

Amoveo smart contracts use the state channels. These contracts can be written in an inbuilt, intuitive, turing complete language. This means that the fees are low, contracts are nearly instant and can support a global audience.

Amoveo also has Oracles. These oracles can learn facts about our world and enforce the outcome of contracts that are governed by these facts. This, for example, can enable users to bet on - the price of the Amazon shares at a time in future or on the outcomes of an election or the actual outcome of a possible adverse event.

The variables that define how to participate in Amoveo can be modified by the Amoveo community using a built-in process.
This way Amoveo will always stay optimally tuned to produce the best results.


[Amoveo whitepaper](docs/white_paper.md).

Amoveo main net was launched at 11:00 AM GMT on March 2, 2018.

## Community
[Amoveo forum on reddit](https://www.reddit.com/r/Amoveo/)

[Amoveo announcements on twitter](https://twitter.com/zack_bitcoin)

[Statistics page to see historic difficulty, and more](https://jimhsu.github.io/amoveo-stats/)

[Network page to see the nodes that power the network](http://185.117.73.74/amoveo-network-status)


## Light node
Simply visit [this webpage](http://159.65.120.84:8080/wallet.html) to participate in Amoveo, no installation necessary.

You can also download the javascript light node to ensure you have the secure version, and to be able to run in cold storage. To open the javascript light node open this file with your browser: `apps/amoveo_http/priv/external_web/wallet.html`

This light node downloads headers and verifies the Proof of Work.
It verifies the merkle proof of all blockchain states you download to ensure security equivalent to a full node, provided you wait for enough confirmations.
You can use the light node to participate in [markets](docs/light_node/market.md).
A light node that currently has markets is [here](http://159.89.106.253:8080/wallet.html).
And the interface to look at the markets is [here](http://159.89.106.253:8080/explorer.html).


## Block Explorer
The block explorer for the network is [here](http://159.65.120.84:8080/explorer.html).

Another explorer is [here](http://159.89.106.253:8080/explorer.html). This one has some markets where you can gamble.


## Full node
[Launch an erlang full node and connect to the network](docs/getting-started/turn_it_on.md)
[Issue commands to your full node](docs/api/commands.md)
Commands such as:
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
The "classic" version only pays a miner when it finds a block.


## Developers

If you want to build on top of Amoveo [read the develper's guide](docs/getting-started/quick_start_developer_guide.md)


## Donations

```
Bitcoin Donations: 1C5Qq5i4uUyEm84GAZ3iAUFgbVAhbCirwj
Veo donations: BGH+3P768A9cSNR3GLSRXgsokSL/Jdbm+rOJogbgiPxq8M+J2R4nVxZ+Hj6WdI4rMsq6nPzkMh77WGBCMx89HUM=
Gifts must be less than $10 000 per person per year.
```



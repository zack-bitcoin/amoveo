Amoveo
==========

Amoveo is a blockchain meant for enforcement of investment and insurance contracts.

Amoveo contracts are enforced using state-channels. This means fees are low, contracts are nearly instant and can support a global audience.

Amoveo has futarchy-based oracle technology.
Amoveo can learn facts about our world and enforce the outcome of contracts that are governed by these facts.
This, for example, enables you to bet on the price of Amazon shares.

The variables that define how to participate in Amoveo can be modified by the Amoveo community using futarchy, a betting-type governance mechanism.
This way Amoveo will always stay optimally tuned to produce the best results.


[Amoveo whitepaper](docs/white_paper.md).

Amoveo main net was launched at 11:00 AM GMT on March 2, 2018.

## Community
[Amoveo forum on reddit](https://www.reddit.com/r/Amoveo/)

[Amoveo announcements on twitter](https://twitter.com/zack_bitcoin)

[Amoveo on Telegram](https://t.me/amoveo)

[Amoveo on Discord. Русский чат. 中文聊天.](https://discord.gg/a52szJw)

[Historic difficulty in depth.](https://amoveo.tools/)

[Amoveo website from the Exantech, the same people who wrote an iphone and android app](https://amoveo.io/)

[Website for exploring oracles and governance variables](https://veo.sh/oracles)
<!---

[Statistics page to see historic difficulty, blocktime, hashrate, and more.](https://jimhsu.github.io/amoveo-stats/)
--->


## Light node

The most secure way to use the light node is to download it from github. https://github.com/zack-bitcoin/light-node-amoveo
This is a cryptoeconomically secure way to use Amoveo.

you can use the light node less securely by clicking [this link](http://159.65.120.84:8080/wallet.html). This is the easiest way to get started.
Using this light node is the same as trusting this server with your money.

An alternative exan.tech made a light node with a different user interface that they host here: Amoveo.exan.tech
Using this light node is the same as trusting them with your money.

This light node downloads headers and verifies the proof of work.
It verifies the merkle proofs for all blockchain state you download to ensure security equivalent to a full node, provided you wait for enough confirmations.
You can use the light node to participate in [markets](docs/light_node/market.md).
A light node that currently has markets is [here](http://159.65.120.84:8080/wallet.html).
And the interface to look at the markets is [here](http://159.65.120.84:8080/explorer.html).


## Places you can use Amoveo smart contracts

[Amoveo Book has a web wallet integrated into the browser for usability](http://amoveobook.com/)


## Block Explorer

[Veopool explorer](http://explorer.veopool.pw/)

[Veoscan explorer. Nodes, blocks, txs, markets, holders, and more.](http://veoscan.io/)

<!---
[mveo explorer. historic difficulty analisys](https://mveo.net/)

[Amoveo.tools historical difficulty chart](https://amoveo.tools/)
--->

The block explorer for the network is [here](http://159.65.120.84:8080/explorer.html).
This explorer can host markets.




## Full node
[Launch an erlang full node and connect to the network](docs/getting-started/turn_it_on.md)

[Issue commands to your full node](docs/api/commands.md)
Commands such as:
* turning the node off without corrupting the database.
* looking up information from the blockchain or it's history.
* making a server that collects fees by routing payments or making markets
* participating in the oracle mechanism or governance mechanism.

## Mining

[This is an open-source miner for AMD and Nvidia GPU. Currently only works with Linux](https://github.com/zack-bitcoin/VeoCL)

[This is a miner. it is for Nvidia or AMD GPUs. It is closed-source.](https://github.com/PhamHuong92/VeoMiner)

[here is another closed source miner](https://github.com/krypdkat/AmoveoMinerMan)

[here is a miner for fpga](https://github.com/dedmarozz/Amoveo-VCU1525-FPGA-Miner/releases)

[Comino appears to be selling some fpga software to mine veo](https://comino.com/shop)

Amoveo's mining algorithm uses SHA256 like bitcoin. But it is a little different, so bitcoin ASICs cannot be used to mine Amoveo.

Full node keys are stored in `_build/prod/rel/amoveo_core/keys/keys.db`


## Mining Pools

http://159.65.120.84:8085/main.html (maintained by Zack, who wrote the Amoveo full node.)

https://amoveopool.com/#getting-started

http://stats.veopool.pw/


## Trading

Be very careful using exchanges. They are centralized, the operator can take all the veo if they wanted.

There are people trading now on discord https://discord.gg/xJQcVaT

Qtrade exchange for BTC-VEO trading: https://qtrade.io/market/VEO_BTC

A1 exchange for ETH-VEO and BTC-VEO trading (previously called amoveo.exchange): https://a1.exchange/


## browser extentions
Firefox. It can be found here. https://addons.mozilla.org/en-US/firefox/addon/amoveo-wallet/ and the source code is here https://github.com/johnnycash77/amoveo3-wallet


## Software to launch a new mining pool

https://gitlab.com/ThatGuy02/nVeoPool an open source mining pool. WARNING!!! this pool has known security vulnerabilities that have not been patched.

[Another open source mining pool](https://github.com/zack-bitcoin/amoveo-mining-pool)


## Developers

If you want to build on top of Amoveo [read the developer's guide](docs/getting-started/quick_start_developer_guide.md)

[Here is an old open-source miner for Nvidia GPU, it works for linux and windows. This software is too slow to be competitive, but it might be useful for educational purposes.](https://github.com/Mandelhoff/AmoveoMinerGpuCuda)


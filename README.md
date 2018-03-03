Amoveo
==========
[Español](docs/es/README.md)

(work in progress) [中文](docs/cn/README.md)


A blockchain for enforcing contracts for investing and insurance. 

Amoveo contracts use state-channel technology. This means fees are low, making an Amoveo contract is nearly instant, and we can support a global audience.

Amoveo has oracle technology.
Amoveo can learn facts about our world and enforce contracts who's outcome is determined by those facts.
This way you can invest in the price of Amazon shares, for example.

The variables that define how to participate in Amoveo can be modified by the Amoveo community using a built-in process.
This way Amoveo will always stay optimally tuned to produce the best results.


[Amoveo whitepaper](docs/white_paper.md).

Amoveo main net will launch at 11:00 AM GMT on March 2, 2018.

## Community
[Amoveo forum on reddit](https://www.reddit.com/r/Amoveo/)

[Amoveo announcements on twitter](https://twitter.com/zack_bitcoin)


## Light node
Simply visit [this webpage](http://159.89.106.253:8080/wallet.html) to participate in Amoveo, no installation necessary.
This light node downloads headers and verifies the proof of work.
It verifies merkle proof of all blockchain state you download to give as much security as a full node, if you wait enough confirmations.
You can mine slowly mine blocks from the light node.


## Block Explorer
The block explorer for the network is [here](http://159.89.106.253:8080/explorer.html)
Go to the block explorer to see all the markets that are being run on that node.


## Full node
[Launch an erlang full node and connect to the network](docs/getting-started/turn_it_on.md)


## Mining
Here is the fastest miner
[Linux and Mac OSX](https://github.com/zack-bitcoin/amoveo-c-miner)
[Windows](https://github.com/Mandelhoff/AmoveoMinerCpu/releases)
It uses SHA256 like bitcoin. But it is a little different, so bitcoin ASICs cannot be used to mine Amoveo.
Full node keys are stored in `_build/prod/rel/amoveo_core/keys/keys.db`


## Mining Pool
If your full node is on a different machine from your miner, you will need a mining pool. [Mining pool software can be found here.](https://github.com/zack-bitcoin/amoveo-mining-pool)


## Developers

If you want to build on top of Amoveo [read the develper's guide](docs/getting-started/quick_start_developer_guide.md)


## Donations

```
Bitcoin Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT
Gifts must be less than $10 000 per person per year.
```



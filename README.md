Amoveo
==========

```
Bitcoin Donations: 1GbpRPE83Vjg73KFvTVZ4EnS2qNkiLY5TT
Gifts must be less than $10 000 per person.
```

A scalable blockchain for financial derivatives.
Optimized for scalability via smart contracts inside state-channels.
Has a build-in oracle for integration with real-world data.
Written in Erlang.

Non-technical [mission statement](docs/mission_statement.md).

[Evidence that Amoveo's goal is technologically possible](docs/design/limit_order_in_channel.md)

[testnet block explorer](http://146.185.142.103:8080/explorer.html)

## Community

[Amoveo forum on reddit](https://www.reddit.com/r/Amoveo/)

[Amoveo announcements on twitter](https://twitter.com/zack_bitcoin)


## Running the blockchain

Launch a node and connect to the network; [look at the quick start guide](docs/getting-started/turn_it_on.md)


## Graphic user interface

[After starting your node, use a web browser on the same computer to visit this website.](http://localhost:8081/login.html)
It is being served by the node you are running.

The block explorer for the network is [here](http://146.185.142.103:8080/explorer.html)
Go to the block explorer to see all the markets that are being run on that node.


## Using the wallet

Amoveo is also a wallet. A wallet is for storing your private key, and for making signatures.
Read here about how to secure your private key with a password. That way you can have assets on Amoveo.
[Secure your keys](docs/api/securing_keys.md)


## Blockchain Commands

[Read about the commands in depth in the docs](docs/api/commands.md) This is how you control the node once it is started. Includes commands for mining, accounts, channels, oracles, and more.


## Testing

We maintain two kinds of tests: unit tests and integration tests. Final test is syncing fresh node with public testnet node.

For tests you need to build one of the test versions of the code instead of the production version. You need to know how to send commands to the test versions. [advanced notes on running any version of the node](docs/getting-started/build_intro.md).

[If you want to run the multi-node tests](/docs/merging-and-testing/testing.md).

[Single-node tests are explained here](/docs/merging-and-testing/unit_testing.md).

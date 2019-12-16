Thank you for your interest in Amoveo.
This doc is an introduction to the technology.
It is made to help you be able to start contributing to the software as quickly as possible.

[Guide to erlang](https://learnyousomeerlang.com/)

## Testing

We maintain two kinds of tests: unit tests and integration tests. Final test is syncing fresh node with public testnet node.

For tests you need to build one of the test versions of the code instead of the production version. You need to know how to send commands to the test versions. [advanced notes on running any version of the node](/docs/getting-started/build_intro.md).

[If you want to run the multi-node tests](/docs/merging-and-testing/testing.md).

[Single-node tests are explained here](/docs/merging-and-testing/unit_testing.md).


## Blockchain Commands

[Read about the commands in depth in the docs](/docs/api/commands.md) This is how you control the node once it is started. Includes commands for accounts, channels, oracles, and more.

## Browser GUI for using wallet integrated into the full node

Now that the browser light node works so well, the browser wallet is only used for testing and as a development tool.

[After starting your node, use a web browser on the same computer to visit this website.](http://localhost:8081/login.html)
It is being served by the node you are running.

The block explorer for the network is [here](http://159.89.87.58:8080/explorer.html)
Go to the block explorer to see all the markets that are being run on that node.


## Wallet integrated into the full node

It is still possible to use the wallet integrated into the node, and it is necessary as a development tool.
Read here about how to secure your private key with a password. That way you can have assets on Amoveo.
[Secure your keys](/docs/api/securing_keys.md)


## transaction types

[This will teach you about the transaction types](/docs/design/transaction_types.md)
Transactions are how you can modify the consensus state of the blockchain.

## database

[This will teach you about trees](/docs/design/trees.md)
Trees are the data structures that hold the consensus state of the blockchain.


[guide to contributing](/docs/contributions.md)

[stuff that needs to be done](/docs/todo.md)

Other concepts that need to be covered:

* hash functions
* merkel trees
* nash equilibrium
* financial derivatives

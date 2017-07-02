Aeternity
==========

A new scalable blockchain, written in Erlang. 

#### Installing dependencies

you will need Erlang and a couple of libraries before you are able to run this software.

[For Ubuntu](docs/ubuntu_dependencies.md)

[For Mac](docs/mac_dependencies.md)


#### Running the blockchain

If you just want to launch a node and connect to the network, [look at the quick start guide](docs/turn_it_on.md)

#### Using the wallet

Included is a wallet. A wallet is for storing your private key, and for making signatures.
Read here about how to secure your private key with a password. That way you can have tokens.
[Secure your keys](docs/securing_keys.md)

### Blockchain Commands

[Read about the commands in depth in the docs](docs/commands.md) This is how you control the node once it is started.

### Testing

To run tests on your machine, run `make tests` in project source root. This will first build, install and start 3 test nodes, then run acceptance tests on them.

If you are looking for more detailed explanation on how the nodes are started look at the [advanced notes on installation](docs/installation_notes.md).

If you want to know more about how the tests are run see [testing](docs/testing.md).

### Else
If you want to know more, get in touch with us via [gitter chat](https://gitter.im/aeternity/Lobby)

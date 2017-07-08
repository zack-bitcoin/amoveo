
Requirements for Aeternity installation cover ```Erlang/OTP (18+)``` and its dependencies

Reporsitory operates using ```rebar3``` and we ship it in the top dir

## Project structure

Project sources live in ```apps``` directory

They get build by ```rebar3``` to ```_build``` directory

```rebar3``` compiles code to ```default``` sub-dir

Depending on profile, from the ```_build/default``` code/libs/apps are symlinked to another nodes in ```_build``` (like dev{1-3}, local, etc.)

When we create tarbal using ```rebar``` ```as``` syntax symlinks are removed and original code goes to self-containing tarball (along Erlang runtime system)

```
_build/
├── default
│   ├── lib
│   └── plugins
├── dev1
│   ├── lib
│   ├── plugins
│   └── rel
├── dev2
│   ├── lib
│   ├── plugins
│   └── rel
├── dev3
│   ├── lib
│   ├── plugins
│   └── rel
└── local
    ├── lib
    ├── plugins
    └── rel
```


## Building releases

Build a release that is capable of syncing with public testnet. (We call it a "production node" in this document.)

```Prod```

``` make prod-build```


Build a release that is good for local one node testing and development

```Local```

``` make local-build```

Build a release that is good for local three node testing and development
Its useful for multinode transactions and payments

```Dev```

``` make test-build```

## Starting your nodes

Starting a test node

``` make test-start ```

Starting a production node

``` make prod-start ```

Starting one of the 3 test nodes

``` make test1-start ``` ``` test2-start ``` ``` test3-start ```

Starting all 3 test nodes at once

``` make test-start ```

## Communicating with your nodes

Attach to production node

```
make prod-attach
```

Attach to test node

```
make local-attach
```

3 different commands to attach to one of the 3 nodes for testing

``` make test1-attach ``` ``` test2-attach ``` ``` test3-attach ```

## Turning off your node

The test node is turned off this way

``` make test-stop ```

The production node is turned off this way

``` make prod-stop ```

An individual of the 3 test nodes can be turned off like this

``` make test1-stop ``` ``` test2-stop ``` ``` test3-stop ```

You can turn off all 3 at once like this

``` make test-stop ```

## Deleting your database to restart from the genesis block.

This preserves your keys.

For the test node

```
make local-clean
```

clean blocks and transactions from production node

```
make prod-clean
```

clean blocks and transactions from one of the 3-nodes.
3 different commands for 3 different nodes

``` make test1-clean ``` ``` test2-clean ``` ``` test3-clean ```

You can also clean all 3 at once

``` make test-clean ```


### Blockchain Commands

[Read about the commands in depth in the docs](commands.md)

#### Sync with the network
To sync with the network and download the blockchain: 
```
sync:start().
```

#### Mining
After fresh install, one can start mining.

To start mining with all CPU cores: 
```
mine:start().
```
To stop mining:
```
mine:stop().
```
to check if you are currently mining:
```
mine:status().
```

#### Spend
```
api:spend(To, Amount).
```

#### Last transactions
```
tx_pool:data().
```


#### Create an account
(does get done automatically when no account and mining starts)
[Make an account](docs/new_account.md)

#### Check your balance
```
easy:balance().
```

#### Stop a node
To stop a node run:
```
easy:off().
```


### Else
If you want to know more, get in touch with us via [gitter chat](https://gitter.im/aeternity/Lobby)
Attach to erlang node

```
make local-release-attach
```

Build and deploy three independent Aeterniy nodes (with the same test master key)

```
make dev-release-unikey
```

Clean test blocks and test data (but preserve the keys config)

```
make dev-release-clean
```


```
_build/
├── default
│   ├── lib
│   └── plugins
├── dev1
│   ├── lib
│   ├── plugins
│   └── rel
├── dev2
│   ├── lib
│   ├── plugins
│   └── rel
├── dev3
│   ├── lib
│   ├── plugins
│   └── rel
└── local
    ├── lib
    ├── plugins
    └── rel
```


### Compiling and Runing

[TODO: check if still valid after rebar]

you will need Erlang and a couple of libraries. Please follow instructions:

[For Ubuntu](docs/compile_ubuntu.md)

[For Mac](docs/compile_mac.md)


### Blockchain Commands

[Read about the commands in depth in the docs](docs/commands.md)

#### Start the blockchain
Start your node with following script:
```
sh start.sh
```

#### Sync with the network
To sync with the network and download the blockchain: 
```
sync:start().
```


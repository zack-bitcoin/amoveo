
Requirements for Aeternity installation cover ```Erlang/OTP (18+)``` and its dependencies

Reporsitory operates using ```rebar3``` and we ship it in the top dir

## Project structure

Project sources live in ```apps``` directory

They get build by ```rebar3``` to ```_build``` directory

```rebar3``` compiles code to ```default``` sub-dir

From the ```_build/default``` code/libs/apps are symlinked to another nodes in ```_build``` (like dev{1-3}, local, etc.)

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
``` make release-build```


Build a release that is good for local one node testing and development

```Local```

``` make test-release-build```

Build a release that is good for local three node testing and development
Its useful for multinode transactions and payments

```Dev```

``` make multi-test-release-build```

## Starting your nodes

Starting a test node

``` make test-release-start ```

Starting a production node

``` make release-start ```

Starting one of the 3 test nodes

``` make start-1 ``` ``` make start-2 ``` ``` make start-3 ```

Starting all 3 test nodes at once

``` make multi-test-release-start ```

## Communicating with your nodes

Attach to production node

```
make release-attach
```

Attach to test node

```
make test-release-attach
```

3 different commands to attach to one of the 3 nodes for testing

``` make attach-1 ``` ``` make attach-2 ``` ``` make attach-3 ```

## Turning off your node

The test node is turned off this way

``` make test-release-end ```

The production node is turned off this way

``` make release-end ```

An individual of the 3 test nodes can be turned off like this

``` make end-1 ``` ``` make end-2 ``` ``` make end-3 ``` 

You can turn off all 3 at once like this

``` make multi-test-release-end ```

## Deleting your database to restart from the genesis block.

This preserves your keys.

For the test node

```
make test-release-clean
```

clean blocks and transactions from production node

```
make release-clean
```

clean blocks and transactions from one of the 3-nodes.
3 different commands for 3 different nodes

``` make clean-1 ``` ``` make clean-2 ``` ``` make clean-2 ```

You can also clean all 3 at once

``` make multi-test-release-clean ```


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
easy:spend(To, Amount).
```
To is the recipient's account ID

#### Last transactions
```
tx_pool:data().
```

#### Find out your account ID
```
keys:id().
```
If it returns something less than 1, that means you don't have an account yet.

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


### Contact

If you want to know more, get in touch with us via [gitter chat](https://gitter.im/aeternity/Lobby)

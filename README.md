Aeternity
==========

A blockchain for trust-free markets in financial derivatives

This is being used by: [æternity](https://aeternity.com).

### Instalation

Requirements for Aeternity instalation cover ```Erlang/OTP (18+)``` and its dependencies

Reporsitory operates using ```rebar3``` and we ship it in the top dir

### Project Commands

Project sources live in ```apps``` directory

They get build by ```rebar3``` to ```_build``` directory

```rebar3``` compiles code to ```default``` sub-dir

From the ```_build/default``` code/libs/apps are symlinked to another nodes in ```_build``` (like dev{1-3}, local, etc.)

When we create tarbal using ```rebar``` ```as``` syntax symlinks are removed and original code goes to self-containing tarball (along Erlang runtime system)

## Build Targets

Build a release that is capable of syncing with public testnet

```Prod```

``` make prod-release```


Build a release that is good for local one node testing and development

```Local```

``` make local-release```

Build a release that is good for local three node testing and development
Its useful for multinode transactions and payments

```Dev```

``` make dev-release-unikey```


##Handy commands


Compile code

```
make compile
```

Create an Erlang release

```
make compile
```

Build 1 node local release

```
make local-release
```

Start the release

```
make local-release-start
```

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
If you want to know more, get in touch with us via [gitter chat](https://github.com/zack-bitcoin/testnet)

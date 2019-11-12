
Requirements for Amoveo installation cover ```Erlang/OTP (18+)``` and its dependencies, and [these dependencies](/docs/getting-started/dependencies.md)

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

``` make multi-build```

## Starting your nodes

Starting a production node

``` make prod-go ```

Starting a test node

``` make local-go ```

Starting one of the 3 test nodes

``` make go1 ``` ``` make go2 ``` ``` make go3```

Starting all 3 test nodes at once

``` make multi-go ```

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

``` make attach1 ``` ``` make attach2 ``` ``` make attach3 ```

## Turning off your node

The test node is turned off this way

``` make local-stop ```

The production node is turned off this way

``` make prod-stop ```

An individual of the 3 test nodes can be turned off like this

``` make stop1 ``` ``` make stop2 ``` ``` make stop3 ```

You can turn off all 3 at once like this

``` make multi-stop ```

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

``` make clean1 ``` ``` make clean2 ``` ``` make clean3 ```

You can also clean all 3 at once

``` make multi-clean ```


### Blockchain Commands
[Read the blockchain terminal interface commands docs](../api/commands.md) for a deeper dive into using the blockchain terminal commands to interface with the blockchain basics, accounts, lookup history, accounts, lookup history, channels, oracles, markets and other interesting functionality.


#### Sync with the network
To sync with the network and download the blockchain: 
```
sync:start().
```


#### Spend
```
api:spend(To, Amount).
```


#### Learn more

If you want to know more, go to the Amoveo Reddit, and see if someone else asked your question. https://www.reddit.com/r/Amoveo



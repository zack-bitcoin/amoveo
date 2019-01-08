### Introduction

Acceptance tests are written in python.

[you can read about single-node tests here](unit_testing.md)


### Basic usage

To run the tests, run `make tests`.
This will:
* kill all running `amoveo` nodes
* build 3 test nodes
* start all 3 tests nodes
* run python tests on these nodes
* kill the nodes

### Detailed usage

You can turn on the 3 test nodes like this:
`make multi-quick`

If the 3 nodes are running, you can run all the integration tests with this command: `python tests/all_tests.py`.

If the 3 nodes are running, you can run all the integration tests individually. For example, here is running the fork tests individually:
`python/fork.py`

To see what is broken, you can look at logs for each of the 3 nodes. The 2nd nodes log is in: `_build/dev2/rel/ae_core/log`

To attach to the 2nd running node and give it commands from the erlang terminal: `make attach2`

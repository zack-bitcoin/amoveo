### Introduction

Acceptance tests are written in `Python 2.x`, using `nose` testing framework.

The only system-wide requirements to run the tests is to have `Python 2.x` and `virtualenv` installed ([virtualenv](https://virtualenv.pypa.io/en/stable/)).

### Test setup

To prepare your environment for acceptance tests, run `make nose-env`.
This will create a `virualenv` in your project directory and install all the necessary libs into it.

### Basic usage

To run the tests, run `make tests`.
This will:
* kill all running `amoveo` nodes
* build 3 test nodes
* start all 3 tests nodes
* run python tests on these nodes
* kill the nodes

### Detailed usage

If you are in development process and have already 3 nodes running, simply run `make python-tests`.
This will run acceptance Python tests on the running nodes.

Command `make python-tests` runs tests suites describes in `nose` tests config file - [nose.cfg](https://github.com/zack-bitcoin/amoveo/blob/master/tests/nose.cfg).

By default, it runs all tests from `tests` directory:

```
[nosetests]
tests=tests
processes=0
process-timeout=60
with-xunitmp=1
verbosity=3
```


To run particular test suite simply change the config file, e.g.:

```
[nosetests]
tests=tests/test_lightning.py
processes=0
process-timeout=60
with-xunitmp=1
verbosity=3
```

You can also always use `nose` commands to run single test suite/class or even single test, e.g.:
```
./bin/nosetests ./tests/test_fork.py
./bin/nosetests ./tests/test_fork.py:ForkTest
./bin/nosetests ./tests/test_fork.py:ForkTest.test_mine_and_sync
```

[you can read about single-node tests here](unit_testing.md)
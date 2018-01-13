### Introduction

Acceptance tests are written in python.

### Basic usage

To run the tests, run `make tests`.
This will:
* kill all running `amoveo` nodes
* build 3 test nodes
* start all 3 tests nodes
* run python tests on these nodes
* kill the nodes

### Detailed usage

If you are in development process and have already 3 nodes running, simply run `python tests/all_tests.py`.
This will run acceptance Python tests on the running nodes.

[you can read about single-node tests here](unit_testing.md)
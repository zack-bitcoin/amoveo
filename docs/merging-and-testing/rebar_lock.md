WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//merging-and-testing/rebar_lock.md)


## Overview

We need a way to guarantee that no accidential changes in dependencies afect the core code. 

One of mechanism to do it, is to lock dependencies to specific hashes representing version.

Our build system (rebar) supports that with lock/unlock api


### Commands

To update rebar lock on local machine.

```
./rebar3 dependency-unlock
make local-build
```
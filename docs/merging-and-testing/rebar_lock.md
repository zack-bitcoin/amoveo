
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
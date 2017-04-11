First install brew, a package manager for OSX.

https://brew.sh/


Next install erlang and rebar, the package manager for erlang
```
brew install erlang
brew install rebar
```

now use rebar to get and compile Aeternity:

```
rebar get
rebar get-deps
rebar compile
```

Now you can run your node with ```sh start.sh```

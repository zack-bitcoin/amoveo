#!/usr/bin/env bash
# this script will be called from initd-aeternity.sh script
# do not call it allone

# start Aeternity Mining Node 3010 and start mining
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3010), keys:unlock(\"abc\"), mine:start(), peers:add({127,0,0,1}, 3020), peers:add({127,0,0,1}, 3030)"
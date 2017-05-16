#!/usr/bin/env bash
# this script will be called from initd-aeternity.sh script
# do not call it allone

erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3020), keys:lock(), peers:add({127,0,0,1}, 3010), peers:add({127,0,0,1}, 3030)"


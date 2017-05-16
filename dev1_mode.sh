#!/usr/bin/env bash
sh clean.sh


erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3010), keys:unlock(\"abc\") "

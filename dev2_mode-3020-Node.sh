#!/usr/bin/env bash
# this script will be called from initd-aeternity.sh script
# do not call it allone
#
# How To?
# Starting testnet with this command:
#
# sh setup_test.sh
#
# Kill all Testnet Server with this command:
#
# sh initd-aeterity.sh stop
#
# READ THIS: https://github.com/Zwilla/aeternity-testnet/blob/master/docs/testnet.md

cp data/keys.db data/keys_dev_mode_backup.db
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3020),
keys:load(\"BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8=\",
\"M/1xsM1DBO82qQcVJVoWVJd4p9YjpwygQJmmYkVLFd8=\",
\"abc\", 3),
keys:unlock(\"abc\") "

# keys:new(\"abc\")"
#, peers:add({46,101,103,165}, 8080)"

#erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3020), keys:lock(), peers:add({127,0,0,1}, 3010), peers:add({127,0,0,1}, 3030)"

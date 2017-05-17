#!/usr/bin/env bash
# this script will be called from initd-aeternity.sh script
# do not call it allone

# start Aeternity Mining Node 3010 and start mining
# erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3010), keys:unlock(\"abc\"), mine:start(), peers:add({127,0,0,1}, 3020), peers:add({127,0,0,1}, 3030)"
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

rm data/*.db;
rm -R blocks;
mkdir -p blocks;
chmod a+x blocks
touch 3010.txt;
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3010),
keys:load(\"BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8=\",
\"M/1xsM1DBO82qQcVJVoWVJd4p9YjpwygQJmmYkVLFd8=\",
\"abc\", 3),
keys:unlock(\"abc\"), peers:add({127,0,0,1}, 3020), peers:add({127,0,0,1}, 3030)";



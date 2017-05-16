#!/usr/bin/env bash
#if you want to use a different port, then start like this:
# sh start 3666

#./rebar get-deps
sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
#./rebar compile #this line checks if any modules were modified, and recompiles them if they were. only needed during testing.
#sh update.sh
#echo "GO TO THIS WEBSITE -------> http://localhost:8041/login.html"
#sleep 1
cp data/keys.db data/keys_dev_mode_backup.db
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3030),
keys:load(\"BOnadmMfDIoCmio3ReSinirULreS3TbCEdr0R6FDDvoVB5xoAJnvwlL3yMgNhBzEb5l36z7bgizw2EKGn0W9rY8=\",
\"M/1xsM1DBO82qQcVJVoWVJd4p9YjpwygQJmmYkVLFd8=\",
\"abc\", 3), 
keys:unlock(\"abc\") "
# keys:new(\"abc\")"
#, peers:add({46,101,103,165}, 8080)"

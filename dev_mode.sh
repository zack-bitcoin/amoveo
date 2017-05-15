#!/usr/bin/env bash
#if you want to use a different port, then start like this:
# sh start 3666

#./rebar get-deps
sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
#./rebar compile #this line checks if any modules were modified, and recompiles them if they were. only needed during testing.
sh update.sh
#echo "GO TO THIS WEBSITE -------> http://localhost:8041/login.html"
#sleep 1
cp data/keys.db data/keys_dev_mode_backup.db
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw(3020),
keys:load(\"BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=\",
\"GMwRk1KJtgJEH2RJp/XVeaQwJ4bpIqAr4lvQcIy4CSQ=\",
\"abc\", 2), 
keys:unlock(\"abc\") "
# keys:new(\"abc\")"
#, peers:add({46,101,103,165}, 8080)"

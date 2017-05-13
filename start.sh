#!/usr/bin/env bash
#if you want to use a different port, then start like this:
# sh start 3666

# sh update.sh

#./rebar get-deps
#sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
#./rebar compile #this line checks if any modules were modified, and recompiles them if they were. only needed during testing.
#echo "GO TO THIS WEBSITE -------> http://localhost:8041/login.html"
#sleep 1


if [[ `uname -s` == `Linux` ]];
then
    echo "check and compile on Linux";
    sh rebar get;
    sh rebar compile;
    fi;

if [[ `uname -s`==Darwin ]]; then
    echo "check and compile on Darwin now! \n";
    sudo ./rebar3 local upgrade
    chmod u+x rebar3;
    chmod u+x *.sh;
    #without sudo we get an error on compiling on osx
    sudo ./rebar3 clean
    sudo ./rebar3 get-deps
    sudo ./rebar3 compile
fi;

exit;
# on Testnet we need this!
# now not deleting the deps folder!
rm yesclean.txt;
sh clean.sh;


#erlc -Ddebug *.erl
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(testnet), serve:pw($1), peers:add({46,101,103,165}, 8080), peers:add({52, 36, 106, 100}, 8080), peers:add({127,0,0,1}, 3010), peers:add({127,0,0,1}, 3020)"

# keys:unlock(\"abc\")"
exit;

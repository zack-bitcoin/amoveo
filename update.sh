#!/usr/bin/env bash

echo "Do you want to pull the master branch? Hit ENTER for NO or wait 5 seconds. Hit y for yes";
read -t 5 pullmaster;

if [[ ${pullmaster} == "y" ]]; then
git pull origin master;
fi;

if [ `uname -s`==Linux ]; then
./rebar3 get-deps;
#./rebar3 get;
./rebar3 compile;
elif [ `uname -s`==Darwin ]; then
./rebar3 get-deps;
./rebar3 compile;
else
    echo "your computer cannot update this";
fi;
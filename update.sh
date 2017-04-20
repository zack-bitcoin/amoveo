#!/usr/bin/env bash

echo "Do you want to pull the master branch? Hit ENTER for no or wait 5 seconds. Hit y for yes";
read -t 5 pullmaster;

if [[ ${pullmaster} == "y" ]]; then
git pull origin master;
fi;

if [ `uname -s`==Linux ]; then
./rebar get-deps;
./rebar get;
./rebar compile;
elif [ `uname -s`==Darwin ]; then
rebar get-deps;
rebar get;
rebar compile;
else
    echo "your computer cannot update this";
fi;
#!/usr/bin/env bash
# Erlang Dialyzer Script to find errors
# copyright michael.padilla@zwilla.de 2017
#
# How to?
#
# just run it and check the dialyzer-result.txt
#
echo "######################### CLEAN APP #########################";
./rebar clean
echo "######################### GET DEPS for ######################";
./rebar get-deps
./rebar -r update-deps
#./rebar delete-deps
#./rebar qc
#./rebar xref
#./rebar shell

echo "######################### COMPILE APP #######################";
./rebar compile

if [ ! -e dialyzer-aeternity.plt ]; then

echo "######################### build the dializer #########################";
# more here: http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html
dialyzer --build_plt --output_plt dialyzer-aeternity.plt --apps erts kernel stdlib mnesia compiler crypto hipe syntax_tools;
echo "######################### ADD APP AETERNITY ##########################";
dialyzer --add_to_plt --output_plt dialyzer-aeternity.plt ebin/*.beam;
echo "######################### CHECK the dializer #########################";
dialyzer --check_plt --plt dialyzer-aeternity.plt;
echo "######################### INFO ABOUT #################################";
dialyzer --plt_info --plt dialyzer-aeternity.plt;
echo "######################### BUILD RESULT OF ############################";
dialyzer --plt dialyzer-aeternity.plt -o dialyzer-result.txt ebin/;
echo "######################### THE RESULT OF ##############################";
cat dialyzer-result.txt;
echo "######################### FINISH BUILD ##############################";
else
echo "######################### checking now #########################";
dialyzer --check_plt --plt dialyzer-aeternity.plt;
# remove old file
rm dialyzer-result.txt;
echo "######################### BUILD RESULT OF ############################";
dialyzer --plt dialyzer-aeternity.plt -o dialyzer-result.txt ebin/;
echo "######################### THE RESULT OF ##############################";
cat dialyzer-result.txt;
echo "######################### FINISH DIALYZER update ##############################";
fi;


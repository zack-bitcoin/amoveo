#!/usr/bin/env bash

echo "######################### CLEAN APP #########################";
./rebar clean
echo "######################### GET DEPS for ######################";
./rebar get-deps
./rebar -r update-deps
echo "######################### COMPILE APP #######################";
./rebar compile

cd ..
echo "######################### RM 3010 #######################";
sudo rm -R aeternity-testnet3010
echo "######################### RM 3020 #######################";
sudo rm -R aeternity-testnet3020
echo "######################### RM 3030 #######################";
sudo rm -R aeternity-testnet3030
echo "######################### CP 3010 #######################";
cp -R aeternity-testnet aeternity-testnet3010
chmod -R 777 aeternity-testnet3010
echo "######################### CP 3020 #######################";
cp -R aeternity-testnet aeternity-testnet3020
chmod -R 777 aeternity-testnet3020
echo "######################### CP 3030 #######################";
cp -R aeternity-testnet aeternity-testnet3030
chmod -R 777 aeternity-testnet3030

echo "######################### RM EBIN 3010/3020/3030 #######################";
rm -R aeternity-testnet3010/ebin
rm -R aeternity-testnet3020/ebin
rm -R aeternity-testnet3030/ebin
echo "######################### RM KEYS 3010/3020/3030 #######################";
rm aeternity-testnet3010/data/keys*
rm aeternity-testnet3020/data/keys*
rm aeternity-testnet3030/data/keys*

echo "######################### COMPILE 3010 #######################";
cd aeternity-testnet3010
./rebar compile
touch 3010.txt
cd ..
echo "######################### COMPILE 3020 #######################";
cd aeternity-testnet3020
./rebar compile
touch 3020.txt
cd ..

echo "######################### COMPILE 3030 #######################";
cd aeternity-testnet3030;
./rebar compile;
touch 3030.txt;
cd ..;


echo "######################### START 3010 #######################";
cd aeternity-testnet3010;
sh initd-aeternity.sh start;
cd ..;

echo "######################### START 3020 #######################";
cd aeternity-testnet3020;
sh initd-aeternity.sh start;
cd ..;

echo "######################### START 3030 #######################";
cd aeternity-testnet3030;
sh initd-aeternity.sh start;
cd ..;

echo "check now with screen command example: \n";
echo "s c r e e n  - r aeternity-3010 \n or this";
echo "s c r e e n  - r aeternity-3020 \n or this";
echo "s c r e e n  - r aeternity-3030 \n ok?";



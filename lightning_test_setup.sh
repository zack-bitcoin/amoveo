#!/usr/bin/env bash
#this sets up the other 3 copies of the software so we can run the lightning test.
# command: lightning_test_setup.sh

rm -r ../FlyingFox1
rm -r ../FlyingFox2
rm -r ../FlyingFox3
cp -r . ../FlyingFox1
cp -r . ../FlyingFox2
cp -r . ../FlyingFox3
sh kill_all_erlang.sh
chmod -R 777 ../FlyingFox1
chmod -R 777 ../FlyingFox2
chmod -R 777 ../FlyingFox3
chmod -R 777 ../aeternity-testnet

chmod -R 777 FlyingFox1
chmod -R 777 FlyingFox2
chmod -R 777 FlyingFox3
chmod -R 777 aeternity-testnet

#sh lightning_test.sh

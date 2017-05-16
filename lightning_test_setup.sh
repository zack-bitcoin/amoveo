#!/usr/bin/env bash
#this sets up the other 2 copies of the software so we can run the lightning test.

rm -r ../FlyingFox2
rm -r ../FlyingFox3
cp -r . ../FlyingFox2
cp -r . ../FlyingFox3
chmod -R 777 ../FlyingFox2
chmod -R 777 ../FlyingFox3

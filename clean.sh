#!/usr/bin/env bash
##############################################################################
#
# @category "AEternity - The Oracle Machine"
# @package "AETERNITY Testnet"
# @author zack.bitcoin@gmail.com
# @link "http://aeternity.com"
# @coauthor Miguel Padilla <miguel.padilla@zwilla.de>
# @copyright (c) 2017 - Miguel Padilla
# @link "https://www.the-internet-of-money.de/aeternity"
#
# According to our dual licensing model, this program can be used either
# under the terms of the GNU Affero General Public License, version 3,
# or under a proprietary license.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
##############################################################################
#
# How to:
#         Step two!
#         only execute this file if you want to delete the keys and blocks at once.
#         This file will be executed normaly from install.sh command.
#
#         This script moves the keys.db to keys_bsackup file removes the data and blocks database

if [ -e "data/keys.db" ]; then
    cp data/keys.db data/keys_backup;
fi;


if [ -d "backup" ]; then
    touch backup/temp.db;
    rm backup/*.db;
else
    mkdir -p backup;
fi;

echo "Do you want to clean the keys and blocks now? Hit enter for YES on enter n for no! Default YES";
read -t 5 cleanall;
if [[ ${cleanall} == "n" ]]; then
echo "No clean testnet";
else
rm data/*.db;
rm blocks/*.db;
fi;


read -t 5 cleanall2;
echo "clean also depencies now? Hit Enter for Yes, or typ n";

if [[ ${cleanall2} == "n" ]]; then
echo "not now!";
rm yesclean.txt;
else

if [ -e yesclean.txt ]; then
rm yesclean.txt;
rm -rf deps;
fi;

fi;

read -t 5 cleanall3;
echo "install now? Hit Enter for NO, or typ y for YES";
# no loop install on automatic mode
if [[ ${cleanall3} == "y" ]]; then
sh install.sh;
else
echo "other sh script do it now if called!";
fi;


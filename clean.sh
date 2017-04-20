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

if [ -e "yesclean.txt" ]; then
rm yesclean.txt;

if [ -e "data/keys.db" ]; then
    cp data/keys.db data/keys_backup;
fi;

rm data/*.db;
rm blocks/*.db;

if [ -d "backup" ]; then
    touch backup/temp.db;
    rm backup/*.db;
else
    mkdir -p backup;
fi;

else

echo "Do you want to clean the keys and blocks now?";
read -t 5 cleanall;
if [ ${cleanall} == "y" ]; then
rm data/*.db;
rm blocks/*.db;
fi;

fi;
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
rm data/*.db
rm blocks/*.db

if [ -d "backup" ]
then
    touch backup/temp.db
    rm backup/*.db
else
    mkdir -p backup
fi

if [ -e "data/keys_backup" ]
then
    cp data/keys_backup data/keys.db
fi

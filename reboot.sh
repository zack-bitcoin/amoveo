#!/usr/bin/env bashbe
##############################################################################
#
# @category "AEternity - The Oracle Machine"
# @package "AETERNITY Testnet"
# @author Miguel Padilla <miguel.padilla@zwilla.de>
# @copyright (c) 2017 - Miguel Padilla
# @link "https://www.the-internet-of-money.de/aeternity"
# @coauthor zack.bitcoin@gmail.com
# @link "http://aeternity.com"
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
#          Inside this script we place all we need to reboot save
#          If a machine crashes we will return save.
#
#

if [ -e "rebootsave.txt" ]; then

# check for updates

# install on reboot

# backup keys and *.db

# last sync before reboot

# whiteliste commands

# set debug mode?

# start automatic

# execute tests on reboot?

  # which tests?

# now reboot save
reboot -f;

fi;
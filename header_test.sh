#!/usr/bin/env bash
##############################################################################
#
# @category "AEternity - The Oracle Machine"
# @package "AETERNITY Testnet"
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
# Command: header_test.sh
if [ $? == 0 ]; then
echo "\n \n \n"
echo "Server is running and all is ok! Check the output\n"
curl -i -d '["header", 0]' localhost:8040
exit
fi

curl -i -d '["header", 0]' localhost:8040
sh header_test.sh | grep '200' &> /dev/null

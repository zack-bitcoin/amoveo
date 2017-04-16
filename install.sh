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

# first install rebar package manager
if [ -e "rebar" ];
then
    echo "rebar already installed \n";

elif [[ `uname -s` == `Linux` ]];
then
    echo "download rebar on Linux";
    wget https://raw.githubusercontent.com/wiki/rebar/rebar/rebar && chmod u+x rebar;

elif [[ `uname -s`==Darwin ]];
then
    curl https://raw.githubusercontent.com/wiki/rebar/rebar/rebar -o rebar;
    chmod u+x rebar;

else
    echo "your system can't compile aeternity at moment! Check our website for more details \n";
fi;

echo "Do you want to check for updates now! Hit ENTER if yes and 'n' if not. \n";
read fetchupdates


if [ "$fetchupdates" == "n" ]; then
echo "OK! We start to compile now! \n"
else
echo "starting Testnet now! \n"
sh update.sh
fi


#use rebar to install other dependencies, explained in rebar.config
sudo rebar get
sudo rebar compile

echo "Do you want to delete the blocks? Hit ENTER if not and 'y' if yes. \n"
read deleteblocks


if [[ "$deleteblocks" == "y" ]]; then
touch yesclean.txt;
echo "clean it now and backup keys";
sh clean.sh #this deletes the database so every time we re-start, we have 0 blocks again. only needed during testing.
echo "deleting blocks ready! \n"

else
echo "not deleting blocks! \n"
fi

echo "Successfully compiled Aeternity testnet \n"

echo "Do you want to start the TestNet now? (y/n) hit ENTER for starting or 'n' to stop now! \n"
read startnow


if [[ "$startnow" == "n" ]]; then
echo "OK installation ready for now! \n"
exit

else
echo "starting Testnet now! \n"
sh start.sh 3010

#sh start.sh 3020
# before we do this we need to copy the project into new folder
fi

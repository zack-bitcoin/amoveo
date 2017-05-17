#!/bin/sh
##############################################################################
 # cloud
 # @category "AETERNITY BLOCKCHAIN"
 # @package "AETERNITY TESTNET"
 # @author Miguel Padilla <miguel.padilla@zwilla.de>
 # @co-author
 # @copyright (c) 2017 - Miguel Padilla
 # @link "https://www.aeternity.de"
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
# READ THIS: https://github.com/Zwilla/aeternity-testnet/blob/master/docs/testnet.md
#
# Command:
# sh initd-aeternity.sh
PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin


	#DAEMON="erl -pa ebin deps/*/ebin/ -eval \"application:ensure_all_started(testnet), serve:pw(3010), keys:unlock(\"abc\"), mine:start()\"";
	DAEMON="aeternityD"
	NAME=aeternity3010;
	DESC="AETERNITY daemon";
	RUNIT="\"application:ensure_all_started(testnet), serve:pw(3010), keys:unlock(\"abc\"), mine:start()\"";
	MINING="mine:start()"

set -e
chmod a+x dev*_mode*.sh;

do_start() {

if [ -f 3010.txt ]; then
    NAME=aeternity3010;

    if [ ! -f config/aeternity.conf ]; then
        echo "ERROR: no aeternity.conf, can't start mine!\n";
		echo "Just running as NODE\n";
		screen -t aeternityT3010 -dmS aeternity-3010 -s ./dev1_mode-3010-Node.sh -q;
	else
	    screen -t aeternityT3010 -dmS aeternity-3010 -s ./dev1_mode-3010-mining.sh -q;
	fi;
	echo "not found!\n";
#screen -t aeternityT3010 -dmS aeternity-3010 -s ./dev1_mode-3010-Node.sh -q;
fi;

	if [ -f 3020.txt ]; then
	NAME=aeternity3020;
	screen -t aeternityT3020 -dmS aeternity-3020 -s ./dev2_mode-3020-Node.sh -q;
	fi;

	if [ -f 3030.txt ]; then
	NAME=aeternity3030;
	screen -t aeternityT3030 -dmS aeternity-3030 -s ./dev3_mode-3030-Node.sh -q;
	fi;
	screen -wipe;

}


do_stop() {
	screen -X -S aeternity-3010 quit;
	screen -X -S aeternity-3020 quit;
	screen -X -S aeternity-3030 quit;

	for i in `ps -ef | grep erl | awk '{print $2}'`; do echo ${i}; kill -9 ${i}; done

	if [ -f 3010.txt ]; then
	echo "not now!\n";
	#rm 3010.txt;
	fi;

	if [ -f 3020.txt ]; then
	echo "not now!\n";
	#rm 3020.txt;
	fi;

	if [ -f 3030.txt ]; then
	echo "not now!\n";
	#rm 3030.txt;
	fi;

	screen -wipe
}
case "$1" in
  start)
        echo "Starting $DESC: "
	do_start
        echo "$NAME"
        ;;
  stop)
        echo "Stopping $DESC: "
	do_stop
        echo "$NAME"
        ;;
  restart|force-reload)
        echo "Restarting $DESC: "
        do_stop
        do_start
        echo "$NAME"
        ;;
  *)
        N=/etc/init.d/$NAME
        echo "Usage: $N {start|stop|restart|force-reload}" >&2
        exit 1
        ;;
esac

exit 0

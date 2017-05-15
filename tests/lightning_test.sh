#!/usr/bin/env bash
#this quickly tests lightning payments. It is a lot faster and easier than using the browser to test the same thing.

#first run lightning_test_setup to copy the software into 2 other directories.
#Open up 3 terminals. 
#Launch one using port 3010, one on 3020, and one on 3030.
#Then run this script from a fourth terminal.

#It lightning spends 4 coins one way, then spends the same 4 back.

curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011


curl -i -d '["create_account", "SlZSdjZTcnFEQ1BpOGZ0RTVB", 10, 2]' http://localhost:3011
sleep 1
curl -i -d '["create_account", "RlpkWGRweGtrenlVS2U1TERW", 10, 3]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 1

curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]' http://localhost:3011
sleep 5
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 1
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]' http://localhost:3021
sleep 5

curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1


curl -i -d '["lightning_spend", [127,0,0,1], 3030, 2, 4, 10]' http://localhost:3011
sleep 1

curl -i -d '["learn_secret", "AgAAAAze1vW4x/rRe4i6nbE="]' http://localhost:3021
sleep 1

curl -i -d '["pull_channel_state", [127,0,0,1], 3030]' http://localhost:3021
sleep 1


#curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3021
#sleep 1
#curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3011
#sleep 1

#curl -i -d '["lightning_spend", [127,0,0,1], 3030, 0, 4]' http://localhost:3021
#sleep 1
#curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3011
#sleep 1
#curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3021
#sleep 1


#this quickly tests lightning payments. It is a lot faster and easier than using the blockchain to test the same thing.

#this test can only work if free_constants:test_mode() is set to true. So the random number generation for the hashlock will be deterministic.

#make sure the code is compiled and can run with `sh start.sh`
#Open up 3 terminals.
#launch `sh dev1_mode.sh` in one, `sh dev2_mode.sh` in the second, and `sh dev3_mode.sh` in the 3rd.
#Then run this script from a fourth terminal.

#It lightning spends 4 coins one way, then spends the same 4 back.

curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011


curl -i -d '["create_account", "SlZSdjZTcnFEQ1BpOGZ0RTVB", 10]' http://localhost:3011
sleep 1
curl -i -d '["create_account", "RlpkWGRweGtrenlVS2U1TERW", 10]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 1

#2 step handshake to make channel
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]' http://localhost:3011
sleep 5
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 1
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]' http://localhost:3021

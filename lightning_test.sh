#this quickly tests lightning payments. It is a lot faster and easier than using the browser to test the same thing.

#first run lightning_test_setup to copy the software into 2 other directories.
#Open up 3 terminals. 
#Copy the Flying Fox install directory so you have 3 copies. 
#Set all 3 passwords to "abc". Use the command keys:new("abc"), then copy keys.db to keys_backup.
#Launch one using port 3010, one on 3020, and one on 3030.
#Then run this script from a fourth terminal.

#It lightning spends 4 coins one way, then spends the same 4 back.

curl -i -d '["key_unlock", "YWJj"]' http://localhost:3011
curl -i -d '["key_unlock", "YWJj"]' http://localhost:3021
curl -i -d '["key_unlock", "YWJj"]' http://localhost:3031

curl -i -d '["keys_id_update", 1]' http://localhost:3021
curl -i -d '["keys_id_update", 2]' http://localhost:3031
curl -i -d '["keys_id_update", 0]' http://localhost:3011

curl -i -d '["create_account", "QkpiMzFWb2U2a0hkWGxFbVRkSzhueGVPeHAvVUs3enJUdFRaMWFWUmxFL2d4TldqMlJlYngrQm1IZ0RuVGU4MHNxMWZSTVBSWGpFNW55N2N3OWU0ckF3PQ==", 10000000, 0]' http://localhost:3011
curl -i -d '["create_account", "QkpiMzFWb2U2a0hkWGxFbVRkSzhueGVPeHAvVUs3enJUdFRaMWFWUmxFL2d4TldqMlJlYngrQm1IZ0RuVGU4MHNxMWZSTVBSWGpFNW55N2N3OWU0ckF3PQ==", 10000000, 0]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021

curl -i -d '["new_channel", [127,0,0,1], 3030, 1000000, 900000, 50]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
curl -i -d '["new_channel", [127,0,0,1], 3030, 500000, 450000, 50]' http://localhost:3021
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011


curl -i -d '["lightning_spend", [127,0,0,1], 3030, 1, 4]' http://localhost:3011
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3021
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3011
sleep 1

curl -i -d '["lightning_spend", [127,0,0,1], 3030, 0, 4]' http://localhost:3021
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3011
sleep 1
curl -i -d '["get_msg", [127,0,0,1], 3030]' http://localhost:3021
sleep 1


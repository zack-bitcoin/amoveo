#First start the two servers:
# sh start.sh 3010
# sh start.sh 3020

#curl -i -d '["key_unlock", "YWJj"]' http://localhost:3011
#curl -i -d '["key_unlock", "YWJj"]' http://localhost:3021

curl -i -d '["keys_id_update", 1]' http://localhost:3011
curl -i -d '["keys_id_update", 2]' http://localhost:3021

curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011


curl -i -d '["mine_block"]' http://localhost:3011
#curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
#curl -i -d '["mine_block"]' http://localhost:3011
#curl -i -d '["sync", [127,0,0,1], 3010]' http://localhost:3021



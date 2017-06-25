#start the two servers:
# sh start.sh 3010
# sh start.sh 3020
# Make sure each server is running from code copied into a different folder. It is important that they each maintain different trie databases, and don't share a trie.

curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011

#push
curl -i -d '["mine_block", 5, 1]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1

#pull
curl -i -d '["mine_block", 5, 1]' http://localhost:3021
sleep 1
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011



#First start the two servers:
# sh start.sh 3010
# sh start.sh 3020
# Make sure each server is running from code copied into a different folder. It is important that they each maintain different trie databases, and don't share a trie.

curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011

curl -i -d '["create_account", "BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=", 1]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1
curl -i -d '["spend", "BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=", 1]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011

curl -i -d '["mine_block"]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011



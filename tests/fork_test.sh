#start the two servers:
# sh start.sh 3010
# sh start.sh 3020

curl -i -d '["mine_block", 10, 1]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3010]' http://localhost:3021
sleep 1


curl -i -d '["mine_block", 2, 1]' http://localhost:3011
curl -i -d '["mine_block", 5, 1]' http://localhost:3021
sleep 1
#curl -i -d '["sync", [127,0,0,1], 3010]' http://localhost:3021 #push
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011 #pull


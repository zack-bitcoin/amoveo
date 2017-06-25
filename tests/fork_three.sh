#start the three servers:
# sh start.sh 3010
# sh start.sh 3020
# sh start.sh 3030

curl -i -d '["mine_block", 10, 1]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3010]' http://localhost:3021
curl -i -d '["sync", [127,0,0,1], 3010]' http://localhost:3031
sleep 1

curl -i -d '["mine_block", 2, 1]' http://localhost:3011
curl -i -d '["mine_block", 5, 1]' http://localhost:3021
sleep 1

curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021 
sleep 1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011

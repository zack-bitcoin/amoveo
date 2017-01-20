
#First start the two servers:
# sh start.sh 3010
# sh start.sh 3020
# Make sure each server is running from code copied into a different folder. It is important that they each maintain different trie databases, and don't share a trie.

curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011

curl -i -d '["mine_block", 1, 1000000]' http://localhost:3011
curl -i -d '["mine_block", 1, 1000000]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1
curl -i -d '["mine_block", 1, 1000000]' http://localhost:3021
sleep 1
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1

curl -i -d '["new_channel", [127,0,0,1], 3020, 1, 10000, 4000, 0, 4]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1
curl -i -d '["mine_block", 1, 1000000]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1
curl -i -d '["channel_spend", [127,0,0,1], 3020, 27]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1

curl -i -d '["channel_bet", [-6,100,105,99,101], [-6, 1234], [127,0,0,1], 3020]' http://localhost:3011 # "dice"

#curl -i -d '["close_channel", [127,0,0,1], 3020]' http://localhost:3011
#sleep 1
#curl -i -d '["mine_block", 1, 1000000]' http://localhost:3021
#sleep 1
#curl -i -d '["sync", [127,0,0,1], 3010]' http://localhost:3021
#sleep 1

#At the end of this test, the channel managers should still be storing the channels as if they were not closed. That way, if we are on a fork and it turns out the channel never got closed, we still have the channel data available to try closing again.
# Channel manager should have some sort of garbage collection to clean up old channels like this.

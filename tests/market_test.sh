#teach the nodes about each other so they can communicate.
curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011

#create another 2 accounts so all three nodes have accounts.
curl -i -d '["create_account", "SlZSdjZTcnFEQ1BpOGZ0RTVB", 10]' http://localhost:3011
sleep 1
curl -i -d '["create_account", "RlpkWGRweGtrenlVS2U1TERW", 10]' http://localhost:3011
sleep 1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1
#create channels so that the 3 nodes are connected by lightning paths.
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]' http://localhost:3011
sleep 5
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 1
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]' http://localhost:3021
sleep 5
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1

#launch a difficulty oracle.

curl -i -d '["new_difficulty_oracle", 20, 0, 1, 7000]' http://localhost:3011 #fee, start, id, difficulty
sleep 1

#make a bet.
curl -i -d '["oracle_bet", 1, 1, 269]' http://localhost:3011 #one higher than the minimum
sleep 1

curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1

curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1

#mine 10 blocks
curl -i -d '["mine_block", 1, 1]' http://localhost:3011
sleep 1

curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 1

curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 1

#settle the difficulty oracle.
#curl -i -d '["oracle_close", 1]' http://localhost:3011
#sleep 1

#launch a question oracle.
#curl -i -d '["new_question_oracle", 20, "aXMgMisyPTQ/", 1, 2]' http://localhost:3011 #fee, start, id, difficulty
#sleep 1

#curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
#sleep 1

#curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
#sleep 1

#create an off-chain market on one of the nodes.
#curl -i -d '["new_market", 2]' http://localhost:3031


#The two nodes should make bets in the market. some but not all of the bets should be matched.

#close the oracle.

#winners collect winnings.

#unmatched trades should be undone.



#curl -i -d '' http://localhost:3011


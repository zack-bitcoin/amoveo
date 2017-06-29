
#create another 2 accounts so all three nodes have accounts.
curl -i -d '["create_account", "OGlqQmhFUks0anBSVHp5Y1lDZGtVTjh0MWg5UDg2YTExMWs3N0RTUDZadUht", 10]' http://localhost:3011
sleep 0.1
curl -i -d '["create_account", "MjFtZk5oeFFNWmphcVFzZzdVTHRZMTlQblN4b2dIQVRkSHg2SjRrSEF2MWdBag==", 10]' http://localhost:3011
sleep 0.1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1
#create channels so that the 3 nodes are connected by lightning paths.
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]' http://localhost:3011
sleep 0.5
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 0.1
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]' http://localhost:3021
sleep 0.5
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1

#launch a difficulty oracle. We need to use a difficulty oracle to measure the expected future difficulty before we are able to run the other kinds of oracles.

curl -i -d '["new_difficulty_oracle", 20, 0, 1, 7000]' http://localhost:3011 #fee, start, id, difficulty
sleep 0.1

#make a bet.
curl -i -d '["oracle_bet", 1, 3, 269]' http://localhost:3011 #one higher than the minimum
sleep 0.1

#mine 10 blocks
curl -i -d '["mine_block", 10, 1]' http://localhost:3011
sleep 0.1

curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1

curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 0.1

#settle the difficulty oracle.
curl -i -d '["oracle_close", 1]' http://localhost:3011
sleep 0.1

#launch a question oracle.
curl -i -d '["new_question_oracle", 0, "aXMgMisyPTQ/", 1, 2]' http://localhost:3011 #fee, start, id, difficulty
sleep 0.1


curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1

curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 0.1

#create an off-chain market on one of the nodes.
curl -i -d '["new_market", 2, 10, 10]' http://localhost:3031


#The two nodes should make bets in the market. some but not all of the bets should be matched.
curl -i -d '["trade", 1000, 1, 1, 2, 20, [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["trade", 3000, 1, 1, 2, 20, [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["trade", 6000, 2, 2, 2, 20, [127,0,0,1], 3030]' http://localhost:3021
curl -i -d '["trade", 8000, 2, 2, 2, 20, [127,0,0,1], 3030]' http://localhost:3021

#match bets
curl -i -d '["market_match", 2]' http://localhost:3031

#settle the oracle.

#curl -i -d '["oracle_bet", 2, 1, 269]' http://localhost:3011 #one higher than the minimum
sleep 0.1
#curl -i -d '["mine_block", 10, 1]' http://localhost:3011
sleep 0.1
#curl -i -d '["oracle_close", 2]' http://localhost:3011
sleep 0.1

#curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1

#curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
sleep 0.1

#oracle better collects winnings in both oracles
#curl -i -d '["oracle_shares", 1]' http://localhost:3011
sleep 0.1
#curl -i -d '["oracle_shares", 2]' http://localhost:3011
sleep 0.1

#oracle better collects unmatched trades
#curl -i -d '["oracle_unmatched", 1, 1]' http://localhost:3011
sleep 0.1
#curl -i -d '["oracle_unmatched", 2, 1]' http://localhost:3011
sleep 0.1


#unmatched trades from off-chain market should be undone.
#winners in off-chain market should collect their winnings.
#losers in the off-chain market should update their channel to reflect the new state.



#curl -i -d '' http://localhost:3011


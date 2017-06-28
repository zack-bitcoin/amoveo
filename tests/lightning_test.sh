#this quickly tests lightning payments. It is a lot faster and easier than using the blockchain to test the same thing.

# This is test for 3 node deployment. It runs in test_mode = true only
# Build 3 nodes using `make dev-release-unikey`
# It will use test master key and corresponding configs

# If you want to attach to all 3 daemons you can do it from 3 terminals
# Then run this script from a fourth terminal.

#It lightning spends 4 tokens one way, then spends the same 4 back.

curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3031
curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3031
curl -i -d '["sync", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011


curl -i -d '["create_account", "SlZSdjZTcnFEQ1BpOGZ0RTVB", 10]' http://localhost:3011
sleep 0.1
curl -i -d '["create_account", "RlpkWGRweGtrenlVS2U1TERW", 10]' http://localhost:3011
sleep 0.1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 0.1

#2 step handshake to make channel
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 1, 10000, 10001, 50, 4]' http://localhost:3011
sleep 0.5
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3021
sleep 0.1
curl -i -d '["new_channel_with_server", [127,0,0,1], 3030, 2, 10000, 10001, 50, 4]' http://localhost:3021
curl -i -d '["sync", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1

curl -i -d '["channel_spend", [127,0,0,1], 3030, 777]' http://localhost:3011
sleep 0.1

curl -i -d '["lightning_spend", [127,0,0,1], 3030, 2, "BAiwm5uz5bLkT+Lr++uNI02jU3Xshwyzkywk0x0ARwY5j4lwtxbKpU+oDK/pTQ1PLz7wyaEeDZCyjcwt9Foi2Ng=", 4, 10]' http://localhost:3011
sleep 0.1

curl -i -d '["pull_channel_state", [127,0,0,1], 3030]' http://localhost:3021
sleep 0.1

curl -i -d '["pull_channel_state", [127,0,0,1], 3030]' http://localhost:3011
sleep 0.1

#curl -i -d '["lightning_spend", [127,0,0,1], 3030, 1, 
#"BMs9FJOY3/h4Ip+lah0Rc4lZDEBbV3wHDZXtqUsWS1kz88bnBr18Q52HnuzdS7IzRuQCU1HVp/AWOnQM6LVcWWw=", 4, 10]' http://localhost:3021
#sleep 1

#curl -i -d '["pull_channel_state", [127,0,0,1], 3030]' http://localhost:3011
#sleep 1

#curl -i -d '["pull_channel_state", [127,0,0,1], 3030]' http://localhost:3021
#sleep 1



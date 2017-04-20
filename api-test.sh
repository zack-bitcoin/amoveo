#!/usr/bin/env bash

curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:3011

curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:3021

curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:3031
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:3031

curl -i -d '["add_peer", [127,0,0,1], 3010]' http://localhost:8041
curl -i -d '["add_peer", [127,0,0,1], 3020]' http://localhost:8041
curl -i -d '["add_peer", [127,0,0,1], 3030]' http://localhost:8041

curl -i -d '["add_peer", [127,0,0,1], 8040]' http://localhost:3011
curl -i -d '["add_peer", [127,0,0,1], 8040]' http://localhost:3021
curl -i -d '["add_peer", [127,0,0,1], 8040]' http://localhost:3031

curl -i -d '["sync", [127,0,0,1], 8040]' http://localhost:3011


curl -i -d '["mine:is_on"]' http://localhost:8040

#geht
curl -i -d '["pubkey"]' http://localhost:8040
curl -i -d '["test"]' http://localhost:8040


curl -i -d '["address"]' http://localhost:8040

curl -i -d '["mine_block", 5, 5]' http://localhost:8040





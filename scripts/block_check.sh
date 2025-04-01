#curl -i --output "Blocks" http://127.0.0.1:3010/blocks/0_1
curl -i --output "Blocks" -d '[-7,0,1]' http://127.0.0.1:3010/blocks
#curl -i -d '["top"]' http://127.0.0.1:3010

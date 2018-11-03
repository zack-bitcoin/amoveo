Amoveo core is frequently updated.
Occasionally there are hard updates that full nodes are required to participate in.
If you forget to update, then your node can end up stuck at one block height, unable to sync.
This happens because a full node of Amoveo is optimized to only verify each block one time. If a block is invalid, then it stores a hash of the block to remember not to check on it again.
If there is an update, then a block that was invalid according to the old rules might become valid after you update.


In that situation you can use this command to tell your full node to give the blocks a second chance, since you have updated:
`block_hashes:second_chance().`
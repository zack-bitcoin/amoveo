download_blocks:send_blocks has a problem.
We should only send the minimum blocks they need, but sometimes we send all the blocks since 0. This will become more of a problem as time goes on.

it should be cleaner to start and stop mining.


maybe nodes need to advertise their own IP/port combo as a peer?


block:check2
We need to check that the time on each block is later than the median of the last finality of blocks.



each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.
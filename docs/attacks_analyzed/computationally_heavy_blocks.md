WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//attacks_analyzed/computationally_heavy_blocks.md)

The transactions can be parallelized a lot.
Here I compute the maximum computational cost of processing a block, assuming we are as parallelized as possible. MAX_COMUTATION_PER_BLOCK

CC = the cost of processing the most expensive channel computation.

S = the cost of processing a tx that doesn't use the VM.

N = the maximum number of txs per block.

MAX_COMUTATION_PER_BLOCK = max((N*S), (CC)).


The channel contracts can never depend on each other's output. So we can parallelize all of them.
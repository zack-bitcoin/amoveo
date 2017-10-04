The transactions can be parallelized a lot.
Here I compute the maximum computational cost of processing a block, assuming we are as parallelized as possible. MAX_COMUTATION_PER_BLOCK

CC = the cost of processing the most expensive channel computation.

S = the cost of processing a tx that doesn't use the VM.

N = the maximum number of txs per block.

MAX_COMUTATION_PER_BLOCK = max((N*S), (CC)).


The channel contracts can never depend on each other's output. So we can parallelize all of them.
extra light nodes do not download blocks. They only download headers.
They consider a tx included once there are enough confirmations.

If an extra light node hears that a certain chain of blocks are bad, then they can download a single bad block and use it to know that that branch is bad.
This is because each block includes with it all the merkle proofs for the consensus state necessary to verify the block. You can verify a single block without knowing anything about the other blocks besides their headers.

This makes it easy for extra light nodes to get back onto the right chain.
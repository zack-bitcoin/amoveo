Blocks Headers and Mining Work
=======

The purpose of this document is to document the relationships between blocks, headers, and the data that miners use when mining.

blocks and headers are represented as erlang records, and these records are defined in [this file](/apps/amoveo_core/src/records.hrl)

The function for computing a header from a block is block:block_to_header defined in [this file](/apps/amoveo_core/src/consensus/chain/block.erl)

To compute the 32-byte hash that miners use when mining, you need to first set the nonce value in the header to 0. then serialize the header according to the function headers:serialize in [this file](/apps/amoveo_core/src/consensus/chain/headers.erl)
The sizes for parts of the header are defined in [this file](/apps/amoveo_core/src/consensus/constants.erl)

When mining, the miner takes the hash of these 32 bytes, appended with 23-bytes of nonce.
H = (32 bytes, hash of header) ++ (23 bytes, random nonce)
H2 = hash(H)
if H2 < difficulty, then it is a valid proof of work.


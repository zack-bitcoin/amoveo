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



Here is an example
```
data0 = {prev_hash,height,timestamp,version,trees_hash,txs_proof_hash,difficulty,nonce,period}
data1 = data0{nonce = 0}
H1 = sha256(data1)
H2 = sha256(H1)
H3 = sha256({H2,nonce})

For block 65238:
["header",65238,"gYhKaAO29pj9T+wo53cYtj5C779GRmtkIy3btvBWmBc=","eppTtlluUgw+EjXeCY6hRLjiNO9i6F3gveh90o4F2As=","d8I94Jkb1JGFSecQPE90IMrouNwud1seF3mpOkhaSDU=",383731482,14058,3,"AAAAAAAAAAAA7PyiZBweAUsAnDQz6BYfzdiXps4AAAA=",1437058057153376419840,5982]

data0 = 81884a6803b6f698fd4fec28e77718b63e42efbf46466b64232ddbb6f05698170000FED60016DF471A00037a9a53b6596e520c3e1235de098ea144b8e234ef62e85de0bde87dd28e05d80b77c23de0991bd4918549e7103c4f7420cae8b8dc2e775b1e1779a93a485a483536EA000000000000000000ecfca2641c1e014b009c3433e8161fcdd897a6ce000000175E
H0 = 05c773d95b61a3f5bb8390948d7ac3ff13e9049e47af709162697f3210d4ba90
Match with https://veoscan.io/block/Bcdz2Vtho%252FW7g5CUjXrD%252FxPpBJ5Hr3CRYml%252FMhDUupA%253D

data1 = 81884a6803b6f698fd4fec28e77718b63e42efbf46466b64232ddbb6f05698170000FED60016DF471A00037a9a53b6596e520c3e1235de098ea144b8e234ef62e85de0bde87dd28e05d80b77c23de0991bd4918549e7103c4f7420cae8b8dc2e775b1e1779a93a485a483536EA0000000000000000000000000000000000000000000000000000000000000000175E
H1 = 15c002bb2f3475775bc61c40b64a73cc83e9c745ce9d2f25eb42cf52baee5b90
H2 = e7d833bc6c383baed9981abc5eb552a4bb8c81ad8e119106edfe5465629932dd
{H2,nonce} = e7d833bc6c383baed9981abc5eb552a4bb8c81ad8e119106edfe5465629932ddecfca2641c1e014b009c3433e8161fcdd897a6ce000000
H3 = 0000000000000134595b4b198fb8f3f59bd45b9e59789f6b59c76fa42870bd8e
```

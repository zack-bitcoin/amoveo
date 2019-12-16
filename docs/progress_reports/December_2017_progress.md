
### Amoveo progress this month

Amoveo's source code became 47% shorter this month. Going from 15k lines to 8k lines.

Amoveo released light nodes in these languages: Chinese, Spanish, Hindi, toki pona

http://159.89.87.58:8080/wallet.html?cn

http://159.89.87.58:8080/wallet.html?sp

http://159.89.87.58:8080/wallet.html?hi

http://159.89.87.58:8080/wallet.html?tp


Amoveo README was simplified.

several vulnerabilities related to pruning were solved.

A document was written about price volatility in Amoveo.

Syncing the light node was automated.

Block syncing is now about 100 times faster than last month. More than 10 blocks per second.

There was a hard fork that changed consensus to waste less space when storing stuff in the merkle tree.

the governance system was documented better, we made final decisions on which variables would be controlled by the governance system.

We hooked up the block size and block reward to the governance system.

renamed a transaction type, and a opcode in chalang to make it easier to understand.

A block miner was written in javascript so it is now possible to mine from the browser.

A block miner was written in C language, it is much faster than any other mining software.

The blockchain was re-written as a light node in javascript. So now there are 2 implementations of Amoveo. You can participate just by visiting a web page. It works from phones too.

http://159.89.87.58:8080/wallet.html

The Chalang VM was re-written to javascript. This was needed for the javascript light node.


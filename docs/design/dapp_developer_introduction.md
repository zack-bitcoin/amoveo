These are the instructions for building new contracts and markets using the embedded language.

 far there is 2 production ready smart contracts I have made. a lightning payment contract, and the market contract. the market one has more tests.
 here is the code that calls the market test:

https://github.com/zack-bitcoin/testnet/blob/master/src/market.erl

this is the market smart contract

https://github.com/zack-bitcoin/testnet/blob/master/src/market.fs

this is a bet contract that is being traded in the market:

https://github.com/zack-bitcoin/testnet/blob/master/src/oracle_bet.fs

the lightning network contract is simpler. it is embedded as a string here:

https://github.com/zack-bitcoin/testnet/blob/master/src/consensus/secrets.erl#L67


You can see more example code, documentation for the compilers, and stuff like that in the chalang repository https://github.com/zack-bitcoin/chalang
## Amoveo White Paper

the purpose of a white paper is to be a technically complete introduction to a project for people without much background in the subject matter.


## abstract
Blockchain is a form of cryptographic database technology. Each blockchain hosts one or more cryptocurrencies, which are a form of wealth that cannot be frozen or confiscated.
Amoveo is a blockchain which secures a cryptocurrency called Veo.
Amoveo has a market system so you can use Veo for executing arbitrary financial strategies.
Using Amoveo you can host a market on a server where you make money every time anyone trades. It is impossible for you to steal money from your customers. 
Amoveo is built with these layers:
1) a blockchain, which makes Amoveo secure.
2) turing complete state channels + oracles, which makes Amoveo scalable and affordable, and it lets the blockchain learn facts about our world.
3) markets, which provide liquidity.
4) financial instruments, which combine many market interactions to execute any financial strategy.


## motivation
Seperating church and state is good. It makes both the church and the state less dangerous.
Combining religious convictions with a monopoly on violence was a deadly combination. It meant that people could be easily convinced to kill you at any time, and there was no consequence to the religious leader who called for your death. 

Seperating finance and state will also be good. It will make both the state and finance less dangerous.
Combining finance with a monopoly on violence means that we can't truly trust the finance system. If the people who are holding our money are easily able to steal our money whenever they want without consequence, this is a terrible combination.
[Amoveo mission statement](design/mission_statement.md)


## block consensus
Amoveo uses Nakamoto consensus, just like bitcoin. It this system, some people act as miners. They receive a reward for producing blocks. Producing a block involves doing an expensive calculation called proof of work. The difficulty of this work is reset every 2000 blocks so that the rate of block production stays about 10 minutes.


## channels
Publishing a transaction to a blockchain is time consuming. You need to wait for the transaction to be included in a block, and then depending on how much money is moved, you need to wait for more blocks to be added to the blockchain for more security. That is why channels were invented.

Channels are 2 party relationships recorded on the blockchain. Each channel has a finite amount of money it controls, just like an account.
Once 2 people have a channel together, they can instantly move the money inside the channel back and forth. This is much faster than publishing a transaction to the blockchain and waiting for confirmations.
When the channel is closed, the money goes back to the accounts that created it according to the final distribution of money in the channel.


## move the smart contracts off-chain into the lightning network
Amoveo smart contracts are built on top of the Amoveo channel system. Amoveo channels allow for bets, which are turing complete contracts written as a part of the channel.
In the case of a disagreement, the blockchain can look at the channel and it's bets and know how to distribute the money from the channel to the participants.

Putting the smart contracts into the channels gives Amoveo many advantages

* speed - off-chain state can be updated as quickly as you can communicate with your channel partner. This is much faster than waiting for confirmations on-chain. If your partner is a server, you can use Amoveo smart contracts instantly.
* privacy - off-chain state can stay secret. As long as neither channel participant disappears or tries to cheat, they never have to publish the contract on-chain.
* scalability- parallel off-chain computation. The channel contracts are usually only processed by the 2 participants in the channel. Every pair of channel participants can be running different channel contracts simultaniously.
* scalability- parallel on-chain transaction computation. Most transactions in a block do not depend on each other. Since there is no on-chain smart contract state, it is easy to calculate which transactions depend on each other. So we can parallelize processing of transactions within the same block on the same machine.


## smart contracts with more than 2 participants
Amoveo uses the hub and spoke model. Each user finds a hub to make a channel with. The user pays the hub a fee to route their payments and bets where they need to go.
Hash time-locking is used to connect bets on multiple channels together. This way users can participate in a smart contract that has more than 2 participants.
If a bet follows a long path, it could be locking up more liquidity than necessary. We can recover the excess in a trust-free way, without interupting the bet.
For example, lets say Alice and Carol made channels with server Bob. Alice and Carol are betting against each other. Alice is betting $1 on the Astros, and Carol is betting $1 on the Cowboys. They are betting at 50-50 odds.
So Alice has at least $1 locked up in her channel, and so does Carol.
Bob has at least $1 locked in Alice's channel, to pay her if she wins. And Bob also needs $1 locked in Carol's channel, to pey her if she wins.
Alice and Carol could make a new channel directly between them, and they could trustlessly move the bet from the long path to the direct path.
This unlocks Bob's $2, so he can use them for something different.


## light nodes and sharding
* light nodes don't download blocks, they only download small headers, and small proofs of the blockchain state.
* each block includes all the proof needed to verify it
- so, blocks can be processed in any order.
* light nodes can mine, which is important for sharding.
* in bitcoin the computational requirement for trust-free access to the blockchain is O((number of blocks) * (transactions per block ~= 2000) * (size of a average tx ~= 1 kb)). This is so expensive that most people cannot afford to run a secure bitcoin node. Or alternatively, you can pay a server to scan the entire UTXO set every time you want to check your balance, but this is too expensive as well, no one even made a server like this yet.
* In amoveo the computational requirement for trust-free access to the blockchain is O((number of blocks) * (size of average header ~= 200 bytes)). So, per block, Amoveo will be able to sync about 10 000 times faster than bitcoin.
* Amove can operate without any full nodes.
* each light node can process as few or as many of the blocks as it wants, and in any order.
* scalability- parallel block computation. Since miners don't have to store any of the consensus state, the blockchain would still be functional under these conditions: it takes more than 10 minutes for a miner to verify a block. The blocktime is 10 minutes. Compare with bitcoin or ethereum, where the time to verify a block needs to be significantly less than the blocktime to maintain security. Ethereum tried to solve this problem with GHOST, but GHOST is only a small improvement, and it comes at the large cost of inflating ETH to pay for uncle blocks. This means that Amoveo can do much more computation per block.


## Smart contract VM
Chalang is the name of the smart contract virtual machine used by Amoveo.

* turing complete - chalang supports functions with recursion.
* merkelized - functions are called by the hash of the contents of the function.
* gas - each operation of the virtual machine consumes a finite resource called gas. If the gas runs out, then the money in the bet is deleted.
* the variable types of chalang are: integers, binary, lists.
* chalang has 2 stacks for storing values during computation. So you can write highly optimized forth-style code.
* chalang has variables for storing values. So you can write easier to read javascript-style code.
* chalang functions are tail call optimized, so you don't have to worry about call stack overflow when you do lots of recursion.
* chalang has a compiler for a forth-like language. The compiler supports macros. This compiler was used to write the market smart contract.


## oracles
show that oracles are a consensus mechanism.
amoveo has a betting-type oracle.


## derivatives, not subcurrencies
Since there is no on-chain state, it would be impossible to put subcurrencies onto Amoveo. Subcurrencies are contrary to the goal of scalability.
Since there are no subcurrencies, it is also impossible to do ICOs on Amoveo, but we do allow for other types of fundraising. 
[I wrote more about subcurrencies and why they are incompatible with channels here.](design/why_not_channels_with_multiple_currencies.md)


## markets
A market provides liquidity you can trade financial risk with other users at the current market price.
Markets on Amoveo are centralized and trust-free.
Each market exists on a server.
The market is secure because the rules are enforced by channel smart contracts.
[State channels are worthless without markets.](design/state_channel_without_off_chain_market.md)
[Here is an explanation of how the market smart contract is written.](docs/design/limit_order_in_channel.md)


## examples use cases
* [fund-raising for creation of public goods](use-cases-and-ideas/insured_crowdfund.md) This is how we will fund further development of Amoveo.
* [prediction markets](use-cases-and-ideas/prediction_market.md) This is how we will plan further development of Amoveo.
* [insurance](use-cases-and-ideas/insurance.md)
* gambling
* [stable coin, also called "synthetic assets"](use-cases-and-ideas/stablecoin.md) This way you can own US dollars on Amoveo.


## governance
Using the oracle, the parameters that define the system can be modified. The mechanism is built with the goal that the oracle will choose parameters that make the value of VEO increase.

Here are all the parameters that can be changed:
block_reward - miner gets this reward when they find a block.
developer_reward - developers get this reward when a block is mined.
time_gas - this is a limit on how many CPU cycles a smart contract can use.
space_gas - this is a limit on how much ram a smart contract can use.
fun_limit - this is how many functions a smart contract can define.
var_limit - this is how many variables a smart contract can define.
oracle_initial_liquidity - the cost to launch an oracle.
minimum_oracle_time
maximum_oracle_time
maximum_question_size - how many bytes big can a question for the oracle be?
block_time_after_median -
channel_closed_time
question_delay
governance_delay
governance_change_limit - this is a limit on how quickly you can modify the governance variables.
fees for each of the different transaction types.


## Chalang opcodes
Chalang is the name of the smart contract virtual machine used by Amoveo.

Here is a list of all 53 opcodes in the language:

int - load the next 4 bytes as an integer.
binary - the next 4 bytes are an integer, this integer says the size of the binary we will load.
print - displays the current contents of the stack to the terminal.
crash - finishes execution of the smart contract, returns the contents of the stack as the output of the smart contract.
nop - does nothing.
fail - stop execution and delete the money from the smart contract.
drop - remove the top item from the main stack.
dup - duplicate the top item on the main stack.
swap - exchange the top 2 things on the main stack.
tuck - ( a b c -- c b a )
rot - ( a b c -- b a c )
ddup - duplicates the top 2 elements on the stack ( a b -- a b a b )
tuckn - ( a b c x 2 -- a x b c )
pickn - ( a b c d 2 -- a b d c )
>r - move the top of the main stack to the return stack.
r> - move the top of the return stack to the main stack.
r@ - copy the top of the return stack to the main stack
hash - take the sha256 of whatever is on top of the stack.
verify_sig - ( sig data pub -- b )
+ - addition ( 11 5 -- 16 )
- - subtraction ( 11 5 -- 4 )
* - multiplication ( 11 5 -- 55 )
/ - division ( 11 5 -- 2 )
gt - greater than ( 11 5 -- 1 )
lt - less than ( 11 5 -- 0 )
pow - exponential ( 9 2 -- 81 ) 
% - remainder ( 12 5 -- 2 )
= - check if they are equal
if - begining of a conditional statement
else - seperate 2 alternatives of conditional statment.
then - ending of a conditional statment
bool_flip - interpret the top of the main stack as a boolean, and flip it.
bool_and - interpret the top 2 elements of the main stack as booleans, and combine them with AND logic.
bool_or - combine with OR logic
bool_xor - combine with XOR logic
stack_size - returns number of elements currently on the stack.
height - number of blocks on the blockchain currently.
gas - check how much time_gas we have left.
ram - check how much space_gas we used up.
many_vars - check how many variables we are using.
many_funcs - check how many functions were defined.
: - start a function definition.
; - finish a function definition.
recurse - call the same function that you are inside of. (This is necessary because functions are named by the hash of their contents, so it would be impossible to do recursion without this opcode.)
call - call the function named at the top of the stack.
! - assign a value to a variable.
@ - look up the value assigned to a variable.
cons - this is how you append an element to a list.
car - this is how you seperate a list into a head element and a tail list.
nil - this is an empty list.
append - this is how you combine 2 lists to make a longer list, or you can use it to append 2 binaries to make a longer binary.
split - this is how you split a list into 2 sublists, or you can use it to split a binary into 2 subbinarys.
reverse - this reverses the order of a list.
is_list - this checks if a value is a list.
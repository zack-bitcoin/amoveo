-module(constants).
-compile(export_all).
%-export([export_all]).
%-define(InitialCoins, round(math:pow(2, 48)) - 1).
%2^74 bits is 25 bitcoin =~ $10,000
%2^64 bits is $10
token_decimals() -> 100000000.
default_port() -> 8040.
key_length() ->
    48. %so at most, we could store 16^11 =~ 17.6 trillion accounts and channels.

initial_coins() -> 1080000000000.
block_reward() -> round(math:pow(2, 29)) - 1.
initial_block_reward() -> round(math:pow(2, 29)) - 1.
initial_difficulty() -> 
%6452.
%4000.%for testing only.
1.
difficulty_bits() -> 24.

hash_size() -> 12.

finality() -> 26.%/docs/security.py explains why.
address_entropy() -> 96.
master_pub() -> <<"BMs9FJOY3/h4Ip+lah0Rc4lZDEBbV3wHDZXtqUsWS1kz88bnBr18Q52HnuzdS7IzRuQCU1HVp/AWOnQM6LVcWWw=">>.
master_address() ->
    testnet_sign:pubkey2address(master_pub()).
max_size() -> 2000000000.%should be 2 gigabytes, does not include old blocks.
gas_limit() -> 1000000.
%200,000,000 is enough to find the first 10001 prime numbers.
max_block_size() -> 200000.%in bytes
%this is only a limit to the size of the transactions.
%the other block parts are also limited. Height must be an integer one greater than the previous.
%prev_hash must be the output of a hash function, which is fixed sized.
%channels is the root of a trie, which is the output of a hash function.
%accounts is the root of a trie.
%mines_block must point to an account id, which is limited, or a tuple of an account id and an address, which is limited in the account:serialize function.
% time must be less than the current time. and greater than 0.
% difficulty must be calculated from the previous difficulty.
% the comment must be less than 140 bytes.
% the magic number is fixed.

%so, the block is limited in size

-define(ConsensusBytePrice, initial_coins() div max_size()).%instead we should have a maximum number of bytes per block, and garbage collect old blocks.
consensus_byte_price() -> ?ConsensusBytePrice.
%-define(MaxAddress, max_size() div 5 div 85).%use about 20% of space to store addresses. Each one is 85 bytes
%max_channel() -> ?MaxChannel.
%-define(MinChannel, constants:initial_coins() div constants:max_channel()).%use about 30% of space to store channels. Each one is 30 bytes
%this constant is also used to determine the minimum amount of money we can put into a channel at a time.
create_channel_fee() -> 0.%consensus_byte_price() * 30.
delete_channel_reward() -> 0.
%decided to charge for accounts based on how long it is open, instead of flat fee.
create_account_fee() -> 0.%consensus_byte_price() * 85.
delete_account_reward() -> 0.%create_account_fee() * 19 div 20. % 95% refund.
%security_ratio() -> fractions:new(3, 2).
%At most, a channel can contain 1/4000th of the money.
initial_channels() -> %Around 10000 channels.
    1.
    %MVB = minimum_validators_per_block(),
    %D = fractions:divide(security_ratio(), security_bonds_per_winner()),
    %fractions:multiply_int(D, 4) div MVB.
%-define(AccountFee, fractions:new(1, max_address() * finality() * 10)).%so if all accounts are full, it takes 10 finalities until most of them start losing so much money that their accounts open up. 

%account_fee() -> ?AccountFee. 
%-define(DelegationFee, fractions:new(finality() * 1000 - 1, finality() * 1000)).%so it would take about 15,000 blocks to lose 1/2 your money. So you have about 350,000 chances to be validator till you lose 1 your money. So you need at least initial_coins()/350000 in delegation to be able to profitably validate. Which means it supports up to 350000 validators at a time max.
%-define(DelegationFee, fractions:new(1, 1000 * finality())).
%delegation_fee() -> ?DelegationFee.
%delegation_reward() -> fractions:subtract(fractions:new(1, 1), ?DelegationFee).
%block_creation_fee() -> 0.
%max_reveal() -> finality()*10.
%1/4000000
%block_creation_fee() -> fractions:new(1, 20000).%Which implies finality only has to be 13 blocks long!!!
%It is important that 1/3 of the block_creation_fee be less than 2/3 of the validator's bond.
%-define(PBCFV, fractions:multiply_int(block_creation_fee(), initial_coins()) div 3).
%-define(BR, fractions:new(1, 1000)).%spending 1000 coins necessarily burns ~1.
%burn_ratio() -> ?BR.
root() -> "data/".
block_hashes() -> root() ++ "block_hashes.db".
keys() -> root() ++ "keys.db".
top() -> root() ++ "top.db".
channel_manager() -> root() ++ "channel_manager.db".
secrets() -> root() ++ "secrets.db".
word_size() -> 100000.
balance_bits() -> 48.%total number of coins is 2^(balance_bits()).
half_bal() -> round(math:pow(2, balance_bits()-1)).
acc_bits() -> hash_size()*8.%total number of accounts is 2^(acc_bits()) 800 billion.
height_bits() -> 32. %maximum number of blocks is 2^this
account_nonce_bits() -> 24.%maximum number of times you can update an account's state is 2^this.
channel_nonce_bits() -> 32.%maximum number of times you can update a channel's state is 2^this.
channel_rent_bits() -> 8.
channel_delay_bits() -> 32. %2^this is the maximum amount of blocks you could have to channel_slash if your channel partner tries to cheat.
orders_bits() -> 32.
account_size() ->    
	((balance_bits() + height_bits() + account_nonce_bits() + acc_bits() + key_length()) div 8) + (2*hash_size()).
channel_size() ->    
    ((key_length() + (acc_bits()*2) + 
	  (balance_bits()*3) + channel_nonce_bits() + 
	  (height_bits()*2) + 
	  channel_entropy() + channel_delay_bits()) div 8) 
	+ 1 + hash_size().
existence_size() -> acc_bits().%hash_length*8

channel_rent() -> account_rent().
account_rent() -> round(math:pow(2, 13)).
%48 bits is max money, 42 bits is initial money.
%if we had a billion accounts, we would want the blockchain to last at least 20 years. 
%144*52*20

retarget_frequency() -> %how many blocks till we recalculate the difficulty
    %40000.
    2000.
block_time() -> 
    600.
    %10.
channel_closed_time() ->
    60*24*60*60 div block_time(). %about 2 months, in blocks
oracle_future_limit() -> %this is a limit of how far in the future an oracle can start.
    60*24*60*60 div block_time(). %about 2 months, in blocks
time_units() -> %1000 = 1 second, 100 = 0.1 seconds
   100. 
start_time() -> 14825749780.
time_bits() -> 32.
% Seconds_130_years = 60*60*24*365.24
% 32 =~ math:log(130*60*60*24*365.24)/math:log(2). 
shares_conversion(Many) -> Many div 7583.
    % half life of 1 year.
    %HalfLife = 365*24*60*60 div block_time(),
    %if blocktime is 6000, then HalfLife is 5256
    %(Ratio^HalfLife) = 1/2,
    %HalfLife*log(Ratio) = log(1/2)
	%Ratio = 2^(log(1/2)/HalfLife)
	%0.99986813137
	%Ratio = 7582/7583
    
    
channel_entropy() -> 16. %Channel contracts only work for a channel with the same 2 account addresses, and with the same channel_entropy that has this many bits.
%this is like another channel nonce, but we only increment it if the channel gets closed and re-created.

fun_limit() -> 1000.
var_limit() -> 10000.

peers() ->
    [].%[{IP, Port}| ...]
comment_limit() -> %When a miner mines a block, they can set this many bytes to whatever they want.
    140.
magic() -> 2.
magic_bits() -> 16.%so we can update it more than 60000 times.
server_ip() -> {46,101,103,165}.
server_port() -> 8080.

block_creation_maturity() ->    
    100.
    %10.%testing
block_time_after_median() ->
    100.
oracle_initial_liquidity() ->
    block_reward() div 2.
day() ->
    24*60*60 div block_time().
governance_delay() ->
    day() div 2.
question_delay() ->
    2*day().
two_days() ->
    2*day().
governance_change_limit() ->
    51.
    
maximum_oracle_time() ->
    minimum_oracle_time()*4.
    
minimum_oracle_time() ->
    %a week in block
    %7*24*60*60 div block_time().
    7.%for testing purposes

maximum_question_size() ->
    1000.
channel_granularity() ->    
    10000.
channel_nonce_space() ->    
    %this is how big the nonce output from a smart contract can be without changing the nonce of the channel.
    1000.

test() ->
    success.



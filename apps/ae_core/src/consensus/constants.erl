-module(constants).
-compile(export_all).
%-export([export_all]).
%-define(InitialCoins, round(math:pow(2, 48)) - 1).
%2^74 bits is 25 bitcoin =~ $10,000
%2^64 bits is $10

key_length() ->
    24. 
address_bits() ->
    hash_size()*8.
    %48. 

pubkey_size()->
    65. %bytes

initial_coins() -> 1080000000000.
block_reward() -> round(math:pow(2, 29)) - 1.
initial_block_reward() -> round(math:pow(2, 29)) - 1.
initial_difficulty() -> 
    case application:get_env(ae_core, test_mode, false) of
	true -> 1;
	_ -> 6452
    end.
difficulty_bits() -> 24.

hash_size() -> 32.

finality() -> 26.%/docs/security.py explains why.
address_entropy() -> hash_size()*8.
master_pub() ->
    {ok, X} = application:get_env(ae_core, master_pub),
    base64:decode(X).

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
%At most, a channel can contain 1/4000th of the money.
initial_channels() -> %Around 10000 channels.
    1.
    %MVB = minimum_validators_per_block(),
%burn_ratio() -> ?BR.
root() -> "data/".
block_hashes() -> root() ++ "block_hashes.db".
keys() -> "keys/keys.db".
top() -> root() ++ "top.db".
channel_manager() -> root() ++ "channel_manager.db".
secrets() -> root() ++ "secrets.db".
order_book() -> root() ++ "order_book.db".
scripts_root() -> "lib/ae_core-0.1.0/priv/".
oracle_bet() -> scripts_root() ++ "oracle_bet.fs".
headers_file() -> root() ++ "headers.db".
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
	%((balance_bits() + height_bits() + account_nonce_bits() + acc_bits() + key_length()) div 8) + (2*hash_size()).
	((balance_bits() + height_bits() + account_nonce_bits()) div 8) + (2*hash_size()) + pubkey_size().
channel_size() ->    
    ((%key_length() + %(address_bits()*2) + 
	  (balance_bits()*3) + channel_nonce_bits() + 
	  (height_bits()*2) + 
	  channel_entropy() + channel_delay_bits()) div 8) 
	+ 1 + (2 * hash_size()) + (2 * pubkey_size()).
existence_size() -> hash_size()*8.

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

comment_limit() -> %When a miner mines a block, they can set this many bytes to whatever they want.
    140.
version() -> 3.
version_bits() -> 16.%so we can update it more than 60000 times.
%rename to "Protocol VERSION".
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



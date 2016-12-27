-module(constants).
-compile(export_all).
%-export([export_all]).
%-define(InitialCoins, round(math:pow(2, 48)) - 1).
%2^74 bits is 25 bitcoin =~ $10,000
%2^64 bits is $10
key_length() ->
    11. %so at most, we could store 16^11 =~ 17.6 trillion accounts and channels.
trie_size() ->
    50000. %we can adjust this many accounts and channels per block.
-define(InitialCoins, round(math:pow(2, 41)) - 1).
initial_coins() -> ?InitialCoins.
block_reward() -> round(math:pow(2, 29)) - 1.
initial_difficulty() -> %1*256.%for testing purposes only
5940.%about 300 seconds on my lenovo AMD Athlon Neo X2 L325 / 1.5 GHz using a single core.
%20*256 ~20 seconds %2 seconds is 16*256

finality() -> 26.%/docs/security.py explains why.
address_entropy() -> 96.
%master_pub() -> <<"QkF4eUUvV2htL1NyMG5PTmJjN2pjaXlBZjhvNHZSZXhOc0ovaVZweVRpMmxTd0lMb0ZJTm1JUjNVdDNpMGRTaEIrd1FzNnA1QStRbmdZeStmTGY4ZzRvPQ==">>.
master_pub() -> <<"BMs9FJOY3/h4Ip+lah0Rc4lZDEBbV3wHDZXtqUsWS1kz88bnBr18Q52HnuzdS7IzRuQCU1HVp/AWOnQM6LVcWWw=">>.
master_address() ->
    testnet_sign:pubkey2address(master_pub()).
max_size() -> 2000000000.%should be 2 gigabytes, does not include old blocks.
gas_limit() -> 1000000.%30,000 is enough for an oracle with 30 elements in the matrix. For example 5 oracle participants and 6 decisions.
%200,000,000 is enough to find the first 10001 prime numbers.
backup() -> fractions:new(19, 20).
%-define(MBS, max_size() div max_reveal() div 10).%use about 10% of size for blocks.
max_block_size() -> 2000000.%2*26 = 52 megabytes of ram to hold blocks.
%-define(ConsensusBytePrice, initial_coins() div max_size()).%instead we should have a maximum number of bytes per block, and garbage collect old blocks.
%$consensus_byte_price() -> ?ConsensusBytePrice.
-define(MaxAddress, max_size() div 5 div 85).%use about 20% of space to store addresses. Each one is 85 bytes
max_address() -> ?MaxAddress.
-define(MaxChannel, max_size() * 3 div 10 div 30).%use about 30% of space to store channels. Each one is 30 bytes
max_channel() -> ?MaxChannel.
-define(MinChannel, constants:initial_coins() div constants:max_channel()).%use about 30% of space to store channels. Each one is 30 bytes
%this constant is also used to determine the minimum amount of money we can put into a channel at a time.
create_channel_fee() -> 0.%consensus_byte_price() * 30.
delete_channel_reward() -> 0.
%decided to charge for accounts based on how long it is open, instead of flat fee.
create_account_fee() -> 0.%consensus_byte_price() * 85.
delete_account_reward() -> 0.%create_account_fee() * 19 div 20. % 95% refund.
security_ratio() -> fractions:new(3, 2).
%At most, a channel can contain 1/4000th of the money.
initial_channels() -> %Around 10000 channels.
    1.
    %MVB = minimum_validators_per_block(),
    %D = fractions:divide(security_ratio(), security_bonds_per_winner()),
    %fractions:multiply_int(D, 4) div MVB.
-define(AccountFee, fractions:new(1, max_address() * finality() * 10)).%so if all accounts are full, it takes 10 finalities until most of them start losing so much money that their accounts open up. 
account_fee() -> ?AccountFee. 
%-define(DelegationFee, fractions:new(finality() * 1000 - 1, finality() * 1000)).%so it would take about 15,000 blocks to lose 1/2 your money. So you have about 350,000 chances to be validator till you lose 1 your money. So you need at least initial_coins()/350000 in delegation to be able to profitably validate. Which means it supports up to 350000 validators at a time max.
-define(DelegationFee, fractions:new(1, 1000 * finality())).
delegation_fee() -> ?DelegationFee.
%delegation_reward() -> fractions:subtract(fractions:new(1, 1), ?DelegationFee).
block_creation_fee() -> 0.
max_reveal() -> finality()*10.
%1/4000000
%block_creation_fee() -> fractions:new(1, 20000).%Which implies finality only has to be 13 blocks long!!!
%It is important that 1/3 of the block_creation_fee be less than 2/3 of the validator's bond.
%-define(PBCFV, fractions:multiply_int(block_creation_fee(), initial_coins()) div 3).
-define(BR, fractions:new(1, 1000)).%spending 1000 coins necessarily burns ~1.
burn_ratio() -> ?BR.
root() -> "data/".
database() -> root() ++ "database.db".
entropy() -> root() ++ "entropy.db".
channel_manager() -> root() ++ "channel_manager.db".
channel_partner() -> root() ++ "channel_partner.db".
accounts() -> root() ++ "accounts.db".
d_accounts() -> root() ++ "d_accounts.db".
blocks() -> root() ++ "blocks.db".
temp() -> root() ++ "temp.db".
block_pointers() -> root() ++ "block_pointers.db".
pointers_start() -> root() ++ "pointers_start.db".
channels() -> root() ++ "channels.db".
d_channels() -> root() ++ "d_channels.db".
keys() -> root() ++ "keys.db".
top() -> root() ++ "top.db".
backup_accounts() -> root() ++ "backup/accounts.db".
word_size() -> 100000.


balance_bits() -> 48.%total number of coins is 2^(balance_bits()).
acc_bits() -> trie_hash:hash_depth()*8.%total number of accounts is 2^(acc_bits()) 800 billion.
height_bits() -> 32. %maximum number of blocks is 2^this
account_nonce_bits() -> 20.%maximum number of times you can update an account's state is 2^this.
channel_nonce_bits() -> 30.%maximum number of times you can update a channel's state is 2^this.
channel_rent_bits() -> 8.
		       
-define(AccountSizeWithoutPadding, (balance_bits() + height_bits() + account_nonce_bits() + acc_bits() + key_length())).
-define(ChannelSizeWithoutPadding, 
	(key_length() + (acc_bits()*2) + 
	     (balance_bits()*2) + channel_nonce_bits() + 
	     (height_bits()*2) + channel_rent_bits() + 
	     1 + 2 + channel_entropy())).
account_padding() ->    
    8 - (?AccountSizeWithoutPadding rem 8).
channel_padding() ->
    8 - (?ChannelSizeWithoutPadding rem 8).
account_size() ->    
    (?AccountSizeWithoutPadding + account_padding()) div 8.
channel_size() ->    
    (?ChannelSizeWithoutPadding + channel_padding()) div 8.

channel_rent() -> account_rent().
account_rent() -> round(math:pow(2, 13)).
%48 bits is max money, 42 bits is initial money.
%if we had a billion accounts, we would want the blockchain to last at least 20 years. 
%144*52*20

retarget_frequency() -> %how many blocks till we recalculate the difficulty
    %40000.
    2000.
block_time() -> 
    6000.
    %10.
time_units() -> %1000 = 1 second, 100 = 0.1 seconds
   100. 
start_time() -> 14825749780.
    
channel_entropy() -> 16. %Channel contracts only work for a channel with the same 2 account addresses, and with the same channel_entropy that has this many bits.

fun_limit() -> 1000.
var_limit() -> 10000.

peers() ->
    [].%[{IP, Port}| ...]
    
    

test() ->
    success.

%(All the money in channels, times this fee) is the amount of money that transfers from delegates who were not elected to delegates who are elected in each block, and gets locked up for finality() blocks. If this number is too high, then poor people can't afford to be validators. If this number is too low, then rich people can't move their money quickly enough.


%<<"BHtLfya6JUNuLXOJ2pGXkyOevYeeyTC5kxzMlB4RTS0DAtqDLxxa0Phb5lBd4oZludcAZzjKXvo8QtdWeJ30gLc=">>.


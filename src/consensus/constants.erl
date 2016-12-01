-module(constants).
-compile(export_all).
%-export([export_all]).
%-define(InitialCoins, round(math:pow(2, 48)) - 1).
%2^74 bits is 25 bitcoin =~ $10,000
%2^64 bits is $10
-define(InitialCoins, round(math:pow(2, 41)) - 1).
initial_coins() -> ?InitialCoins.
initial_difficulty() -> 256*160.
finality() -> 26.%/docs/security.py explains why.
address_entropy() -> 92.
master_pub() -> <<"BMDq+C+mI+Al4f5WHtq93IrCDaCrq0VU6+BxRaRZ0CgPjGppwu6nIg654GQGx8jQWwvVkjvODtDpKayUInZiOJQ=">>.
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
all_secrets() -> root() ++ "all_secrets.db".
blocks() -> root() ++ "blocks.db".
temp() -> root() ++ "temp.db".
block_pointers() -> root() ++ "block_pointers.db".
pointers_start() -> root() ++ "pointers_start.db".
channels() -> root() ++ "channels.db".
d_channels() -> root() ++ "d_channels.db".
keys() -> root() ++ "keys.db".
secrets() -> root() ++ "secrets.db".
backup_accounts() -> root() ++ "backup/accounts.db".
word_size() -> 100000.

test() ->
    success.

%(All the money in channels, times this fee) is the amount of money that transfers from delegates who were not elected to delegates who are elected in each block, and gets locked up for finality() blocks. If this number is too high, then poor people can't afford to be validators. If this number is too low, then rich people can't move their money quickly enough.


%<<"BHtLfya6JUNuLXOJ2pGXkyOevYeeyTC5kxzMlB4RTS0DAtqDLxxa0Phb5lBd4oZludcAZzjKXvo8QtdWeJ30gLc=">>.


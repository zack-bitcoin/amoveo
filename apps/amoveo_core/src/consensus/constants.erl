-module(constants).
-compile(export_all).
root0() -> 1.
key_length() -> 24. 
address_bits() -> hash_size()*8.
pubkey_size()-> 65. %bytes
initial_coins() -> 10000000000. %100 coins
%27017593349040. %about 1 year.
initial_fee() -> 
    governance:tree_number_to_value(encoded_fee()).
encoded_fee() -> 905.
initial_trading_fee() -> 936. %about 200000. it is out of 1 veo, or 100000000, so this is 0.2%
    
    
initial_difficulty() -> 
    %to run single-node tests, it works with 10.
    %for integration market tests, works with 2500.
    case application:get_env(amoveo_core, kind) of
        {ok, "local"} -> 10;%unit tests
        {ok, "integration"} -> 2500;%integration tests.
%10;
        {ok, "production"} -> 8844
    end.
difficulty_bits() -> 24.
hash_size() -> 32.
address_entropy() -> hash_size()*8.
master_pub() ->
    case application:get_env(amoveo_core, kind) of
        {ok, "production"} -> 
	    <<4,189,18,206,25,5,24,85,181,145,52,221,156,239,44,26,
	     124,15,19,53,47,199,101,54,159,33,2,193,105,148,36,244,
	     97,47,22,207,60,175,158,167,199,152,51,25,83,197,83,191,
	     194,116,18,229,105,172,24,130,156,172,243,251,252,92,53,
	     89,87>>; % base64 encoded, this is: <<"BL0SzhkFGFW1kTTdnO8sGnwPEzUvx2U2nyECwWmUJPRhLxbPPK+ep8eYMxlTxVO/wnQS5WmsGIKcrPP7/Fw1WVc=">>
	_ ->
	    {ok, X} = application:get_env(amoveo_core, master_pub),
	    base64:decode(X)
    end.

compressed_burn() ->
    %H = hash:doit(<<"burn">>).
    %trees2:compress_pub(trees2:decompress_pub(<<0, H/binary>>)).
    <<6,133,159,172,197,164,201,184,10,194,238,247,137,22,193,
      149,59,204,202,171,96,20,187,17,185,222,131,55,67,14,
      163,79,12>>.
decompressed_burn() ->
<<4,133,159,172,197,164,201,184,10,194,238,247,137,22,193,
  149,59,204,202,171,96,20,187,17,185,222,131,55,67,14,
  163,79,12,44,95,249,84,73,140,186,4,206,150,209,152,247,
  25,106,181,8,250,101,41,54,169,238,228,188,0,126,155,31,
  238,166,138>>.
    
    
    
custom_root_location() ->
    case application:get_env(amoveo_core, files) of
        undefined -> "";
        {ok, X} -> X
    end.
        
custom_root() -> 
    {ok, T} = application:get_env(amoveo_core, kind),
    case T of
        "production" -> custom_root_location();
        _ -> ""
    end.
            
keys() -> custom_root() ++ "keys/keys.db".

root() -> custom_root() ++ "data/".
nc_sigs() -> root() ++ "nc_sigs.db".
headers_file() -> root() ++ "headers.db".
headers_file2() -> root() ++ "headers2.db".
headers_file3() -> root() ++ "headers3.db".
headers_file4() -> root() ++ "headers4.db".
headers_file5() -> root() ++ "headers5.db".
block_hashes() -> root() ++ "block_hashes.db".
block_db_dict() -> root() ++ "block_db_dict.db".
block_db_dict2() -> root() ++ "block_db_dict2.db".
block_db2_dict() -> root() ++ "block_db2_dict.db".
block_db2_dict2() -> root() ++ "block_db2_dict2.db".
block_db3_dict() -> root() ++ "block_db3_dict.db".
checkpoints() -> root() ++ "checkpoints.db".
top() -> root() ++ "top.db".
recent_blocks() -> root() ++ "recent_blocks.db".
blocks_file2() -> root() ++ "blocks.db".
blocks_file3() -> root() ++ "blocks3.db".
blocks_file4() -> root() ++ "blocks4.db".
blocks_file() -> custom_root() ++ "blocks/".

scripts_root() -> "lib/amoveo_core-0.1.0/priv/".
scalar_oracle_bet() -> scripts_root() ++ "scalar_oracle_bet.fs".
oracle_bet() -> scripts_root() ++ "oracle_bet.fs".

channels_root() -> custom_root() ++ "channels/".
oracle_questions_file() -> channels_root() ++ "oracle_questions.db".
secrets() -> channels_root() ++ "secrets.db".
order_book() -> channels_root() ++ "order_book.db".
channel_manager() -> channels_root() ++ "channel_manager.db".
arbitrage() -> channels_root() ++ "arbitrage.db".

word_size() -> 100000.
balance_bits() -> 48.
half_bal() -> round(math:pow(2, balance_bits()-1)).
acc_bits() -> hash_size()*8.%total number of accounts is 2^(acc_bits()) 800 billion.
height_bits() -> 32. %maximum number of blocks is 2^this
account_nonce_bits() -> 24.%maximum number of times you can update an account's state is 2^this.
channel_nonce_bits() -> 32.%maximum number of times you can update a channel's state is 2^this.
channel_rent_bits() -> 8.
channel_delay_bits() -> 32. %2^this is the maximum amount of blocks you could have to channel_slash if your channel partner tries to cheat.
orders_bits() -> 32.
sub_account_size() ->
    ((balance_bits() + account_nonce_bits()) div 8) + 4 + pubkey_size() + hash_size().
account_size() ->
	((balance_bits() + account_nonce_bits()) div 8) + (hash_size()) + pubkey_size().
%6 + 3 + 32 + 65 = 106.
contract_size() ->
    ((balance_bits() +
          channel_nonce_bits() + 
          height_bits() + 
          channel_delay_bits()) div 8) + 
        1 + 2 + (4 * hash_size()) + 2.
market_size() ->
    (3 * hash_size()) + 
        (2*2) + 
        (3 * (balance_bits() div 8)).
trade_size() ->
    hash_size() + (height_bits() div 8).
receipt_size() ->
    (hash_size()) + (height_bits() div 8)
        + pubkey_size().

channel_size() ->    
    (((balance_bits()*3) + channel_nonce_bits() + 
      (height_bits()) + 
      channel_delay_bits()) div 8) 
	+ 1 + (hash_size()) + (2 * pubkey_size()).
retarget_frequency() -> %2000. %how many blocks till we recalculate the difficulty
    case application:get_env(amoveo_core, kind) of
        %{ok, "local"} -> 12;%unit tests
        {ok, "local"} -> 12;%unit tests
        {ok, "integration"} -> 12;%integration tests
        {ok, "production"} -> 2000
    end.
time_units() -> 100. % 0.1 seconds
start_time() -> 15192951759.
%14825749780. % (os:system_time() div (1000000 * constants:time_units())).
time_bits() -> 40.
version_bits() -> 16.%so we can update it more than 60000 times.
period_bits() -> 16. %so the maximum block time is about 109 minutes
%server_ip() -> {139,59,144,76}.
server_ip() -> {139,223,85,216}.
server_port() -> 8080.
channel_granularity() -> 10000.

developer_lock_period() ->%in seconds
    60*60*24*365.
    
oracle_question_liquidity() ->
    1200.

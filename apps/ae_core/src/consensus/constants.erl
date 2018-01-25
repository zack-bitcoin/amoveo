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
encoded_fee() -> 783.
    
initial_difficulty() -> 
    case application:get_env(ae_core, kind) of
        {ok, "local"} -> 0;%unit tests
        {ok, "integration"} -> 1000;%2500;%integration tests.
	{ok, "production"} -> 6452
    end.
difficulty_bits() -> 24.
hash_size() -> 32.
address_entropy() -> hash_size()*8.
master_pub() ->
    {ok, X} = application:get_env(ae_core, master_pub),
    base64:decode(X).
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
oracle_questions_file() -> root() ++ "oracle_questions.db".
recent_blocks() -> root() ++ "recent_blocks.db".
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
	((balance_bits() + account_nonce_bits()) div 8) + (hash_size()) + pubkey_size().
channel_size() ->    
    (((balance_bits()*3) + channel_nonce_bits() + 
      (height_bits()) + 
      channel_delay_bits()) div 8) 
	+ 1 + (hash_size()) + (2 * pubkey_size()).
retarget_frequency() -> %2000. %how many blocks till we recalculate the difficulty
    case application:get_env(ae_core, kind) of
        %{ok, "local"} -> 12;%unit tests
        {ok, "local"} -> 12;%unit tests
        {ok, "integration"} -> 100;%integration tests
        {ok, "production"} -> 2000
    end.
time_units() -> 100. % 0.1 seconds
start_time() -> 14825749780.
time_bits() -> 32.
version_bits() -> 16.%so we can update it more than 60000 times.
period_bits() -> 16. %so the maximum block time is about 109 minutes
server_ip() -> {159,89,106,253}.
server_port() -> 8080.
channel_granularity() -> 10000.

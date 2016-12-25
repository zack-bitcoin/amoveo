-module(free_constants).
%These constants can be different on every node in the network. You can adjust these variables to suit your own situation.
-compile(export_all).
hashlock_time() -> 30.
max_channel() -> constants:initial_coins() div 100000.
max_message_size() -> 10000.
inbox_per_peer() -> 100.
liquidity_ratio() -> fractions:new(2, 3).%if a user is willing to put 100 coins into a channel, then the server is willing to put 200 in.
minimum_tx_fee() ->
    constants:initial_coins() div 1000000000000.
fork_tolerance() ->    
   %this is how long of a fork we can recover from. If this number is bigger, it takes longer to sync with the network because you download more unnecessary blocks.
    20.

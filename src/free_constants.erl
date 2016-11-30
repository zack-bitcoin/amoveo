-module(free_constants).
-compile(export_all).
hashlock_time() -> 30.
max_channel() -> constants:initial_coins() div 100000.
max_message_size() -> 10000.
inbox_per_peer() -> 100.
liquidity_ratio() -> fractions:new(2, 3).%if a user is willing to put 100 coins into a channel, then the server is willing to put 200 in.
    

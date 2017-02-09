-module(free_constants).
%These constants can be different on every node in the network. You can adjust these variables to suit your own situation.
-compile(export_all).
hashlock_time() -> 30.
channel_delay() ->
    100.
max_channel() -> constants:initial_coins() div 100000.
max_message_size() -> 10000.
inbox_per_peer() -> 100.
liquidity_ratio() -> fractions:new(2, 3).%if a user is willing to put 100 coins into a channel, then the server is willing to put 200 in.
tx_fee() -> %when you make a tx, this is the fee you spend by default. 
    10.
minimum_tx_fee() ->%only txs with this fee or higher get accepted into your mempool. If you are a miner, you are censoring all txs with lower fees.
    constants:initial_coins() div 1000000000000.
fork_tolerance() ->    
   %this is how long of a fork we can recover from. If this number is bigger, it takes longer to sync with the network because you download more unnecessary blocks.
    %It is best to keep this number low when you first sync, and make it bigger once you are synced with the network.
    %on nodes that are mining, this should probably be set very low. 
    20.
min_channel_ratio() ->
    %So the customer needs to put in twice as much money as the server.
    0.5.
    %{f, 1, 2}.
bets() -> %tuple list. {Name, BetFile}
    [
     {dice, "src/bets/dice.fs"}
    ].
gas_limit() ->
    constants:gas_limit().
time_limit() ->
    %maximum number of miliseconds to wait for a channel contract to process.
    %if this number is too high, then it is easy to
    100000.
space_limit() ->
    100000.
    
vm(SS, State) ->
    chalang:vm(SS, time_limit(), space_limit(), constants:fun_limit(), constants:var_limit(), State).

min_channel_delay() -> 4.
max_channel_delay() -> 100.


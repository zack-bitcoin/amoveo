-module(free_constants).
%These constants can be different on every node in the network. You can adjust these variables to suit your own situation.
-compile(export_all).


tx_fee() -> %when you make a tx, this is the fee you spend by default.
    {ok, TxFee} = application:get_env(ae_core, tx_fee),
    TxFee.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%% Below are variables never used in the code (or used in commented out code). %%
%% These may be probably removed.                                              %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

max_channel() -> constants:initial_coins() div 100000.
liquidity_ratio() -> ae_core_fractions:new(2, 3).%if a user is willing to put 100 coins into a channel, then the server is willing to put 200 in.
bets() -> %tuple list. {Name, BetFile}
    %% XXX: is it used? is it correct after my change (priv dir)?
    [
     {dice, code:priv_dir(ae_core) ++ "/bets/dice.fs"}
    ].

vm(SS, State) ->
    {ok, TimeLimit} = application:get_env(ae_core, time_limit),
    {ok, SpaceLimit} = application:get_env(ae_core, space_limit),
    {ok, FunLimit} = application:get_env(ae_core, fun_limit),
    {ok, VarLimit} = application:get_env(ae_core, var_limit),
    chalang:vm(SS, TimeLimit, SpaceLimit, FunLimit, VarLimit, State).

fork_tolerance() ->
    50.

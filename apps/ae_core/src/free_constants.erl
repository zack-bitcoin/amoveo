-module(free_constants).
%These constants can be different on every node in the network. You can adjust these variables to suit your own situation.
-compile(export_all).


tx_fee() -> %when you make a tx, this is the fee you spend by default.
    {ok, TxFee} = application:get_env(ae_core, tx_fee),
    TxFee.

-module(nonce_check).
-export([doit/2]).

doit(NonceCheck, Nonce) ->
    if
        NonceCheck ->
            true = is_integer(Nonce),
            Nonce;
        true -> none
    end.

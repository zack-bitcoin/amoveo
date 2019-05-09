-module(testnet_sign).
-export([test/0,test2/1,sign_tx/3,sign/2,verify_sig/3,shared_secret/2,verify/1,data/1,
	 empty/1,empty/0,
	 verify_1/2,verify_2/2,
	 new_key/0, new_key/1
]).
-record(signed, {data="", sig="", sig2=""}).
empty() -> #signed{}.
empty(X) -> #signed{data=X}.
data(X) -> X#signed.data.
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, de(Pub), de(Priv), params())).
generate() -> crypto:generate_key(ecdh, params()).
new_key() -> %We keep this around for the encryption library. it is used to generate 1-time encryption keys.
    generate(). %returns {Pub, Priv}
new_key(X) when ((size(X) == 32) and is_binary(X)) ->
    crypto:generate_key(ecdh, params(), X).
    
sign(S, Priv) -> sign:sign(S, Priv).
verify_sig(S, Sig, Pub) -> sign:verify_sig(S, Sig, Pub).
verify_1(Tx, Pub) -> 
    verify_sig(Tx#signed.data, Tx#signed.sig, Pub).
verify_2(Tx, Pub) -> 
    verify_sig(Tx#signed.data, Tx#signed.sig2, Pub).
verify_both(Tx, Addr1, Addr2) ->
    X = verify_1(Tx, Addr1),
    Y = verify_2(Tx, Addr1),
    if
        X -> verify_2(Tx, Addr2);
        Y -> verify_1(Tx, Addr2);
        true -> false
    end.
type_check(Type) -> %these are the types that get signed twice
    lists:any(fun(X) -> X==Type end, [gc, nc, ctc, ctc2, spk]).
verify(SignedTx) ->
    Tx = SignedTx#signed.data,
    N1 = element(2, Tx),
    Type = element(1, Tx),
    B = type_check(Type),
    if
	B ->
	    N2 = element(3, Tx),
	    verify_both(SignedTx, N1, N2);
	true -> verify_1(SignedTx, N1)
    end.
sign_tx(SignedTx, Pub, Priv) when element(1, SignedTx) == signed ->
    Tx = SignedTx#signed.data,
    N = element(2, Tx),
    if
	(N == Pub)-> 
	    Sig = sign(Tx, Priv),
	    SignedTx#signed{sig=Sig};
	true ->
	    %make sure Tx is one of the types that has 2 signatures: tc, ctc, gc
	    Type = element(1, Tx),
	    true = type_check(Type),
	    N2 = element(3, Tx),
	    if
		(N2 == Pub) ->
		    Sig = sign(Tx, Priv),
		    SignedTx#signed{sig2=Sig};
		true -> {error, <<"cannot sign">>}
	    end
	 end;
sign_tx(Tx, Pub, Priv) ->
    Sig = sign(Tx, Priv),
    N = element(2, Tx),
    N2 = element(3, Tx),
    ST = if
	(N == Pub) -> 
	    #signed{data=Tx, sig=Sig};
	(N2 == Pub) ->
	    #signed{data=Tx, sig2=Sig};
	true -> {error, <<"cannot sign">>}
    end,
    ST.



test() ->
    %{Address, Pub, Priv} = hard_new_key(), %recomputing is too slow. better to write it down, and reuse it each time.
    {Pub, Priv} = new_key(),
    {Pub2, Priv2} = new_key(),
    %X = <<1,2,3,4>>,
    X = {abc, 3,6,3},
    Sig = sign(X, Priv),
    true = verify_sig(X, Sig, Pub),
    Acc = accounts:new(Pub, 0),
    Acc2 = accounts:new(Pub2, 0),
    %Binary = address2binary(Pub),
    %Pub = binary2address(Binary),
    Root0 = constants:root0(),
    Accounts1 = accounts:write(Acc, 1),
    Accounts = accounts:write(Acc2, Accounts1),
    Tx = {ctc, Pub, Pub2},
    Signed1 = sign_tx(Tx, Pub, Priv), 
    Signed = sign_tx(Signed1, Pub2, Priv2),
    Signed2 = sign_tx({spend, Pub, 0, Pub2, 1, 1}, Pub, Priv),
    Verbose = false,
    if
	Verbose ->
            io:fwrite("pubkeys: 1 ~s, 2 ~s", [Pub, Pub2]),
            io:fwrite("privkeys: 1 ~s, 2 ~s", [Priv, Priv2]),
            io:fwrite("signed tx: ~s", [packer:pack(Signed)]);
	true -> ok
    end,
    true = verify(Signed2),
    true = verify(Signed),
    true = verify_both(Signed, Pub, Pub2),
    true = verify_both(Signed, Pub2, Pub),
    false = verify_both(Signed, Pub, Pub),
    %true = valid_address(Pub),
    success.
times(0, _) -> ok;
times(N, F) ->
    F(),
    times(N-1, F).
test2(X) ->
    times(X, fun() -> generate() end ).

-module(testnet_sign).
-export([test/0,test2/1,sign_tx/4,sign/2,verify_sig/3,shared_secret/2,verify/2,data/1,
	 empty/1,empty/0,
	 verify_1/2,verify_2/2, 
	 %pubkey2address/1, valid_address/1, 
	 new_key/0,pub/1,pub2/1%,address2binary/1,binary2address/1
]).
-record(signed, {data="", sig="", pub = "", sig2="", pub2=""}).
pub(X) -> X#signed.pub.
pub2(X) -> X#signed.pub2.
empty() -> #signed{}.
empty(X) -> #signed{data=X}.
data(X) -> X#signed.data.
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, de(Pub), de(Priv), params())).
generate() -> crypto:generate_key(ecdh, params()).
new_key() -> %We keep this around for the encryption library. it is used to generate 1-time encryption keys.
    {Pub, Priv} = generate(),%crypto:generate_key(ecdh, params()),
    {Pub, Priv}.
sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, term_to_binary(S), [Priv, params()])).
verify_sig(S, Sig, Pub) -> 
    SD = de(Sig),
    PD = Pub,
    crypto:verify(ecdsa, sha256, term_to_binary(S), SD, [PD, params()]).
verify_1(Tx, Pub) -> 
    Pub == Tx#signed.pub,
    verify_sig(Tx#signed.data, Tx#signed.sig, Pub).
verify_2(Tx, Pub) -> 
    Pub == Tx#signed.pub2,
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
    lists:any(fun(X) -> X==Type end, [gc, nc, ctc, spk]).
	%(Type == gc) or (Type == nc) or (Type == ctc) or (type == spk)

verify(SignedTx, Accounts) ->
    Tx = SignedTx#signed.data,
    N1 = element(2, Tx),
    %{_, Acc1, _Proof} = accounts:get(N1, Accounts),
    Type = element(1, Tx),
    B = type_check(Type),
    if
	B ->
	    N2 = element(3, Tx),
	    %{_, Acc2, _Proof2} = accounts:get(N2, Accounts),
	    verify_both(SignedTx, N1, N2);
	true -> 
	    verify_1(SignedTx, N1)
    end.
sign_tx(SignedTx, Pub, Priv, _) when element(1, SignedTx) == signed ->
    Tx = SignedTx#signed.data,
    N = element(2, Tx),
    %{_, Acc, _Proof} = accounts:get(N, Accounts),
    if
	(N == Pub)-> 
	    Sig = sign(Tx, Priv),
	    SignedTx#signed{sig=Sig, pub=Pub};
	true ->
	    %make sure Tx is one of the types that has 2 signatures: tc, ctc, gc
	    Type = element(1, Tx),
	    true = type_check(Type),
	    N2 = element(3, Tx),
	    %{_, Acc2, _Proof2} = accounts:get(N2, Accounts),
	    %BAddr = accounts:addr(Acc2),
	    if
		(N2 == Pub) ->
		    Sig = sign(Tx, Priv),
		    SignedTx#signed{sig2=Sig, pub2=Pub};
		true -> {error, <<"cannot sign">>}
	    end
	 end;
sign_tx(Tx, Pub, Priv, _) ->
    Sig = sign(Tx, Priv),
    N = element(2, Tx),
    N2 = element(3, Tx),
    ST = if
	(N == Pub) -> 
	    #signed{data=Tx, sig=Sig, pub=Pub};
	(N2 == Pub) ->
	    #signed{data=Tx, sig2=Sig, pub2=Pub};
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
    %io:fwrite(Sig),
    true = verify_sig(X, Sig, Pub),
    Acc = accounts:new(Pub, 0, 0),
    Acc2 = accounts:new(Pub2, 0, 0),
    %Binary = address2binary(Pub),
    %Pub = binary2address(Binary),
    Accounts1 = accounts:write(0, Acc),
    Accounts = accounts:write(Accounts1, Acc2),
    Tx = {ctc, Pub, Pub2},
    Signed1 = sign_tx(Tx, Pub, Priv, Accounts), 
    Signed = sign_tx(Signed1, Pub2, Priv2, Accounts),
    Signed2 = sign_tx({spend, Pub, 0, Pub2, 1, 1}, Pub, Priv, Accounts),
    Verbose = false,
    if
	Verbose ->
	    io:fwrite("pubkeys\n"),
	    io:fwrite(Pub),
	    io:fwrite("\n"),
	    io:fwrite(Pub2),
	    io:fwrite("\n"),
	    io:fwrite("privkeys\n"),
	    io:fwrite(Priv),
	    io:fwrite("\n"),
	    io:fwrite(Priv2),
	    io:fwrite("\n"),
	    io:fwrite("signed tx\n"),
	    io:fwrite(packer:pack(Signed)),
	    io:fwrite("\n");
	true -> ok
    end,
    true = verify(Signed2, Accounts),
    true = verify(Signed, Accounts),
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
		     


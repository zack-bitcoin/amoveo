-module(testnet_sign).
-export([test/0,test2/1,sign_tx/4,sign/2,verify_sig/3,shared_secret/2,verify/2,data/1,
	 %revealed/1,
	 empty/1,empty/0,
	 %set_revealed/2,
	 verify_1/2,verify_2/2, 
	 %pubkey2address/1, valid_address/1, 
	 %hard_new_key/0,
	 new_key/0
	 %pub/1,pub2/1,address2binary/1,binary2address/1
	]).
-record(signed, {data="", sig="", sig2=""}).
-define(cs, 8). %checksum size
empty() -> #signed{}.
empty(X) -> #signed{data=X}.
data(X) -> X#signed.data.
%set_revealed(X, R) -> 
%    X#signed{revealed = R}.
%revealed(X) -> X#signed.revealed.
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, Pub, Priv, params())).
%to_bytes(X) -> term_to_binary(X).
to_bytes(X) -> term_to_binary(X).
generate() -> crypto:generate_key(ecdh, params()).
new_key() -> %We keep this around for the encryption library. it is used to generate 1-time encryption keys.
    %{Pub, Priv} = generate(),%crypto:generate_key(ecdh, params()),
    generate().
    %{en(Pub), en(Priv)}.
sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, term_to_binary(S), [Priv, params()])).
verify_sig(S, Sig, Pub) -> 
    PS = constants:pubkey_size(),
    PS = size(Pub),
    SD = de(Sig),
    crypto:verify(ecdsa, sha256, term_to_binary(S), SD, [Pub, params()]).
verify_1(Tx, Pub) -> 
    %Pub = Tx#signed.pub,
    verify_sig(Tx#signed.data, Tx#signed.sig, Pub).
verify_2(Tx, Pub) -> 
    %Pub2 = Tx#signed.pub2,
    verify_sig(Tx#signed.data, Tx#signed.sig2, Pub).
verify_both(Tx, Pub1, Pub2) ->
    X = verify_1(Tx, Pub1),
    Y = verify_2(Tx, Pub1),
    if
        X -> verify_2(Tx, Pub2);
        Y -> verify_1(Tx, Pub2);
        true -> false
    end.
type_check(Type) -> %these are the types that get signed twice
    lists:any(fun(X) -> X==Type end, [gc, nc, ctc, spk]).
	%(Type == gc) or (Type == nc) or (Type == ctc) or (type == spk)

verify(SignedTx, Accounts) ->
    Tx = SignedTx#signed.data,
    Pub1 = element(2, Tx),
    {_, Acc1, _Proof} = accounts:get(Pub1, Accounts),
    Type = element(1, Tx),
    B = type_check(Type),
    if
	B ->
	    Pub2 = element(3, Tx),
	    {_, Acc2, _Proof2} = accounts:get(Pub2, Accounts),
	    verify_both(SignedTx, accounts:pubkey(Acc1), accounts:pubkey(Acc2));
	true -> 
	    verify_1(SignedTx, accounts:pubkey(Acc1))
    end.
sign_tx(SignedTx, Pub, Priv, Accounts) when element(1, SignedTx) == signed ->
    
    %Pub = base64:decode(Pub64),
    Tx = SignedTx#signed.data,
    Sig = sign(Tx, Priv),
    %N = element(2, Tx),
    HP = element(2, Tx),
    %HP2 = testnet_hasher:doit(Pub),
    {_, Acc, _Proof} = accounts:get(HP, Accounts),
    Pub0 = accounts:pubkey(Acc),
    if
	(Pub0 == Pub) -> 
	    SignedTx#signed{sig=Sig};
	true ->
	    %make sure Tx is one of the types that has 2 signatures: tc, ctc, gc
	    Type = element(1, Tx),
	    true = type_check(Type),
	    N2 = element(3, Tx),
	    {_, Acc2, _Proof2} = accounts:get(N2, Accounts),
	    CheckPub2 = accounts:pubkey(Acc2),
	    if
		(CheckPub2 == Pub) ->
		    SignedTx#signed{sig2=Sig};
		true -> {error, <<"cannot sign">>}
	    end
	 end;
sign_tx(Tx, Pub, Priv, Accounts) ->
    Sig = sign(Tx, Priv),
    N = element(2, Tx),
    {_, Acc, _Proof} = accounts:get(N, Accounts),
    CheckPub = accounts:pubkey(Acc),
    if
	(Pub == CheckPub) ->
	    #signed{data=Tx, sig=Sig};
	true -> 
	    N2 = element(3, Tx),
	    {_, Acc2, _Proof} = accounts:get(N2, Accounts),
	    CheckPub2 = accounts:pubkey(Acc2),
	    if
		(Pub == CheckPub2) ->    
		    #signed{data=Tx, sig2=Sig};
		true -> {error, <<"cannot sign">>}
	    end
    end.

-define(AddressEntropy, constants:address_entropy()).

test() ->
    {Pub, Priv} = new_key(),
    {Pub2, Priv2} = new_key(),
    X = {abc, 3,6,3},
    Sig = sign(X, Priv),
    true = verify_sig(X, Sig, Pub),
    Acc = accounts:new(Pub, 0, 0),
    Acc2 = accounts:new(Pub2, 0, 0),
    io:fwrite("testnet sign 0\n"),
    Accounts1 = accounts:write(0, Acc),
    io:fwrite("testnet sign 1\n"),
    Accounts = accounts:write(Accounts1, Acc2),
    io:fwrite("testnet sign 2\n"),
    Tx = {ctc, Pub, Pub2},
    io:fwrite("testnet sign 3\n"),
    Signed1 = sign_tx(Tx, Pub, Priv, Accounts), 
    io:fwrite(packer:pack(Signed1)),
    io:fwrite("testnet sign 4\n"),
    Signed = sign_tx(Signed1, Pub2, Priv2, Accounts),
    io:fwrite("testnet sign 5\n"),
    Signed2 = sign_tx({spend, Pub, 0, 0, Pub2, 1, 1}, Pub, Priv, Accounts),
    io:fwrite("signed "),
    io:fwrite(packer:pack(Signed)),
    io:fwrite("\n"),
    io:fwrite("testnet sign 6\n"),
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
    success.
%hard_new_key() ->
%    {Pub, Priv} = new_key(),
    %Address = pubkey2address(Pub),
%    {Address, Pub, Priv}.
times(0, _) -> ok;
times(N, F) ->
    F(),
    times(N-1, F).
test2(X) ->
    times(X, fun() -> generate() end ).
%test3() ->
%    timer:tc(sign, hard_new_key, []).
		     


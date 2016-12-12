-module(encryption).
-export([test/0,bin_enc/2,bin_dec/2,send_msg/4,get_msg/2,msg/1,id/1]).
-record(msg, {sig = "", msg = "", id = 0}).
-record(emsg, {key = "", msg = ""}).
msg(Msg) -> Msg#msg.msg.
id(Msg) -> Msg#msg.id.

si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
bin_enc(Key, Bin) ->
    {_, X} = crypto:stream_encrypt(si(Key), Bin),
    X.
bin_dec(Key, Msg) ->
    {_, Y} = crypto:stream_decrypt(si(Key), Msg),
    Y.
sym_enc(Key, Msg) -> bin_enc(Key, term_to_binary(Msg)).
sym_dec(Key, Emsg) -> binary_to_term(bin_dec(Key, Emsg)).
send_msg(M, ToPub, FromPub, FromPriv) -> 
    {EphPub, EphPriv} = testnet_sign:new_key(),
    Msg = #msg{sig=testnet_sign:sign(EphPub, FromPriv), msg=M, id = FromPub},
    SS = testnet_sign:shared_secret(ToPub, EphPriv),
    io:fwrite(packer:pack(Msg)),
    Emsg = sym_enc(SS, Msg),
    #emsg{key=EphPub, msg=base64:encode(Emsg)}.
get_msg(Msg, Priv) ->
    Sig = sym_dec(testnet_sign:shared_secret(Msg#emsg.key, Priv), base64:decode(Msg#emsg.msg)),
    %Acc = block_tree:account(Sig#msg.id),
    io:fwrite(packer:pack(Sig)),
    true = testnet_sign:verify_sig(Msg#emsg.key, Sig#msg.sig, Sig#msg.id),
    Sig.

test() ->
    {Pub, Priv} = sign:new_key(),
    {Pub2, Priv2} = sign:new_key(),
    Val = <<"1234">>,
    Binary = <<2,3,4>>,
    true = bin_dec("abc", bin_enc("abc", Val)) == Val,
    true = bin_dec("abc", bin_enc("abc", Binary)) == Binary,
    Record = {f, Binary},
    Sig = get_msg(send_msg(Record, Pub2, Pub, Priv), Priv2),
    true = Sig#msg.msg == Record,
    success.

test2() ->
    {Pub, Priv} = sign:new_key(),
    Val = <<"1234">>,
    Binary = <<2,3,4>>,
    true = bin_dec("abc", bin_enc("abc", Val)) == Val,
    true = bin_dec("abc", bin_enc("abc", Binary)) == Binary,
    Record = {f, Binary},
    P = keys:pubkey(),
    FromPriv = <<"as7i9Cbpq3cuov9pwklxVtHmB2J2NVWvKNif9XrpGVrs4weWesYgYfjUcgtqHhiszQX+0eDkzHR3EKYB">>,
    SM = send_msg(Record, Pub, P, FromPriv),
    Sig = get_msg(SM, Priv),
    io:fwrite(Sig),
    true = Sig#msg.msg == Record,
    success.

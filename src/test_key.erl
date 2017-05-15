-module(test_key).
-export([test/0]).

test() ->
    {NewAddr,NewPub,NewPriv} = testnet_sign:hard_new_key(),
    Pub = keys:pubkey(),
    Priv = keys:shared_secret(Pub),
    {Pub, Priv} = crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)),
    success.
    

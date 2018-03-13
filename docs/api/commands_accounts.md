Commands related to accounts
=========

#### Find out your account pubkey in binary format
```
api:pubkey().
```
It returns pub key that identifies an account

#### Check your balance
```
api:balance().
```

Your encrypted private key is stored in
`_build/prod/rel/amoveo_core/keys/keys.db`
Save a copy.

#### Spend to account pubkey `To`
```
api:spend(To, Amount).
api:spend(base64:decode(<<"BBH26TpQgvscsPWyfIz3zjSA4wopZrVKf3mYktTd2xnjOYi/MW5AXODhK4ZZnud2DeRFkyVlq9q5zESFqbWJCE8=">>), 123).
```
this send 123 satoshis = 0.00000123 coins


####Create Account
[WARNING!!! Before creating an account, make sure your wallet is secure!](keys.md)
To create a new account, and give it some money:
```
api:create_account(Address, AmountOfMoney).
```

####Delete Account
To delete an account and send all it's money to account ID:
```
api:delete_account(Pub).
```

####Look up an account
```
api:account(Pub).
```

#### check mempool to see if tx was created correctly
```
tx_pool:get().
```

#### share your txs with peer P
```
P = lists:nth(2, peers:all()).
api:txs(P).
```
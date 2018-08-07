Commands related to accounts
=========

#### Find out your account pubkey in external base64 format
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

[WARNING!!! make sure your wallet is secure!](keys.md)
#### Spend to account pubkey `To`
```
api:spend(To, Amount).
api:spend(base64:decode(<<"BBH26TpQgvscsPWyfIz3zjSA4wopZrVKf3mYktTd2xnjOYi/MW5AXODhK4ZZnud2DeRFkyVlq9q5zESFqbWJCE8=">>), 123).
```
this send 123 satoshis = 0.00000123 Veo


#### Multi-spend
This is used to make one tx that spends veo to multiple recipients.
This spends 500 satoshis to one account, and 450 to another:
```
api:spend([{500, base64:decode(<<"BBH26TpQgvscsPWyfIz3zjSA4wopZrVKf3mYktTd2xnjOYi/MW5AXODhK4ZZnud2DeRFkyVlq9q5zESFqbWJCE8=">>)},{450, base64:decode("BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=")}]).
```

####Delete Account
To delete an account and send all it's money to account ID:
```
api:delete_account(Pub).
```
WARNING! do not reuse a pubkey after it has been deleted.

####Look up an account
```
api:account(Pub).
```

#### check mempool to see if tx was created correctly
```
tx_pool:get().
```

### look up transaction history of an account in the last 100 blocks.
```
api:address_history(Pubkey, 100, api:height()).
```

#### share your txs with peer P
```
P = lists:nth(2, peers:all()).
api:txs(P).
```
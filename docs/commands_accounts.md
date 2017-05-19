Commands related to accounts
=========

#### Find out your account ID
```
keys:id().
```
If it returns something less than 1, that means you don't have an account yet.


### Find out your address:
```
keys:address().
```

#### Check your balance
```
easy:balance().
```

#### Spend to account ID `To`
```
easy:spend(To, Amount).
```

####Create Account
[WARNING!!! Before creating an account, make sure your wallet is secure!](new_account.md)
To create a new account, and give it some money:
```
easy:create_account(Address, AmountOfMoney).
```

####Delete Account
To delete an account and send all it's money to account ID:
```
easy:delete_account(ID).
```

####Look up an account
```
easy:account(ID).
```
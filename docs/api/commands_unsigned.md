WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//api/commands_unsigned.md)

This page shows how to make unsigned transactions, which are suitable for cold storage applications.


How to spend tokens to an existing account.
`spend_tx:make_dict(To, Amount, Fee, From).`
spends 0.01 Veo, fee of 0.00153 Veo
```
Tx = spend_tx:make_dict(base64:decode("BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4="), 1000000, 153000, base64:decode("BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=")).
rp(packer:pack(Tx)).
```

How to make a new account for someone who only has an address and doesn't have an account.
`create_account_tx:make_dict(Pub, Amount, Fee, From).`
gives 0.01 Veo, fee of 0.00153 Veo
```
Tx = create_account_tx:make_dict(bae64:decode("BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4="), 1000000, 153000, base64:decode("BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=")).
rp(packer:pack(Tx)).
```
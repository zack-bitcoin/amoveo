* What is the difference between id and adress

An ID is an integer. it is used to look up an account in the database. The database stores addresses and balances. Addresses cannot be used to look up accounts in the database.
The address is the hash of your public key. It is used to verify that your signature was created using your private key.


* how would one go about getting the transactions of an address

To get the transactions created by an address you would have to look at every block, and check if that address made an payment. This is not easy to do by default.
`PBlock = block:read_int(4).` would look up the 4th block_plus and store it in a variable.
`block:txs(block:block(PBlock)).` returns the transactions spent in that block.
Each transaction type has a different command for checking who spent that transaction. you can look up commands to deal with transactions in /src/consensus/txs/

* if you are running a node and you query the balance of another address?

To query the balance of another address is difficult. You would need to look up every account until you found the account that stored the matching address.
Querying the balance of an id is easy. do this: `accounts:balance(api:account(ID)).`


u have
ids
accounts
addresses

[9:52]
and i cant figure out the functionality of each.

zack
[9:52 PM]
ids are the integers for looking up accounts. when you send money to someone, you need to know their id.

[9:52]
an account is the part of the on-chain state that records the address and balance of one of the users.

[9:53]
an address is the hash of a pubkey.

The terminal interface to the oracle
=============


New question oracle
====
This oracle asks a true/false question about the future. Eventually, the answer to this question will get recorded on the oracle, and will be accessible to the smart contracts.
```
api:new_question_oracle(Start, Question).
```

New governance oracle
====
This oracle updates the variables that define the blockchain protocol. 
```
api:new_governance_oracle(GovName, GovAmount).
```

You can look up current governance values like this:
```
trees:get(governance, GovName).
```

See all oracles where betting has started, and betting has not yet finished.
====
```
oracles:ready_for_bets().
```
returns a list like [{Question1, Oracle1}, {Question2, Oracle2}...].


See all oracles where it is possible to close the oracle.
====
```
oracles:ready_to_close().
```
returns a list like [{Question1, Oracle1}, {Question2, Oracle2}...].


See all existing oracles
====

```
oracles:all().
```
returns a list like [{Question1, Oracle1}, {Question2, Oracle2}...].
You can use this to find out the OID for an oracle.

look up an oracle by id
=====
```
trees:get(oracles, ID).
```

The order of things stored in the oracle datastructure is defined in [the records.hrl file](https://github.com/zack-bitcoin/amoveo/blob/master/apps/amoveo_core/src/records.hrl#L62)

Bet in an oracle
====
type is one of the atoms in this list: [true, false, bad]
You can either bet that the answer to the question is true or false, or you can bet that it is a bad question.
```
api:oracle_bet(OracleID, Type, Amount).

```
Type is either 1, 2, or 3.
1 = true
2 = false
3 = bad question

or for a scalar oracle:
```
api:minimum_scalar_oracle_bet(OID, N).
```
where N is an integer between 0 and 1023 inclusive representing the value being measured by this oracle.
This trick only works if all the 10 binary oracles making up this scalar oracle do not have bets.

Look up unmatched bets in the oracle
====
```
api:orders(OracleID).
```

Close an oracle
====
If the oracle has had the same output state for a long enough period of time, then this is how anyone can close the channel and end the betting period.
```
api:oracle_close(OracleID).
```

Oracle Shares
====
Collect shares purchased in an oracle
```
api:oracle_winnings(OracleID).
```
or, if it is a scalar oracle
```
api:scalar_oracle_winnings(OracleID).
```

Oracle unmatched
====
If you had unmatched trades sitting in the order book when the oracle closed, this is how you get your money back.
```
api:oracle_unmatched(OracleID).
```
or, if it is a scalar oracle
```
api:scalar_oracle_unmatched(OracleID).
```


Creating a scalar oracle
====
A scalar oracle is actually multiple binary oracles. Each one is for 1 bit of the scalar value.
Many is how many bits make up the scalar value. set it to 10 for now.
set OID to be a random 32-byte value. This will be the ID of the 0th bit. the next oracles will occur at OID+1, OID+2...
```
api:new_scalar_oracle(Start, Question, OID, Many).
```
Use api:oracle_bet and api:oracle_close to close each of the 10 oracles. then use api:oracle_winnings and api:oracle_unmatched to get your money back out of them.



Markets
====
read about how to launch a market on a single full node here:
[market](commands_market.md)

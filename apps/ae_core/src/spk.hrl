
-record(spk, {acc1,acc2, 
	      bets, space_gas, time_gas, 
	      cid, amount = 0, nonce = 0,
	      delay = 0
	     }).
-record(bet, {code, amount, 
              key,%key is instructions on how to re-create the code of the contract so that we can do pattern matching to update channels.
              meta}).%meta is {direction_we_bet, maxprice}
-record(ss, {code, prove, meta = 0}). %meta is the price being matched at.

-type height() :: non_neg_integer().
-record(header, {height :: height(),
                 prev_hash,
                 trees_hash,
                 txs_proof_hash,
                 time,
                 difficulty,
                 version,
                 nonce,
                 accumulative_difficulty = 0,
                 period}).

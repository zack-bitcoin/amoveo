
-record(key, {pub, id}). %used for shared, oracle_bets, and orders
-record(spk, {acc1,acc2, 
	      bets, space_gas, time_gas, 
	      cid, amount = 0, nonce = 0,
	      delay = 0
              %spk needs one more value.
              %The spk can only be used if the channel nonce is >= this number.
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
%things needed for long-term storage
-record(block, {height,%LTS
                prev_hash,%LTS
                trees_hash,%LTS
                time,%LTS
                difficulty,%LTS
                period,%LTS
                version,%LTS
                nonce = 0,%LTS
                trees,
                txs,%LTS
                prev_hashes = {prev_hashes},
                proofs = [],%LTS
                roots,%LTS
                hash = <<>>,
		market_cap = 0,%LTS
		channels_veo = 0,%LTS
		live_channels = 0,%LTS
		many_accounts = 1,%LTS
		many_oracles = 0,%LTS
		live_oracles = 0}).%LTS

%cd is the channel data for one channel.
-record(cd, {me = [], %me is the highest-nonced SPK signed by this node.
	     them = [], %them is the highest-nonced SPK signed by the other node. 
	     ssme = [], %ss is the highest nonced ScriptSig that works with me
	     ssthem = [], %ss is the highest nonced ScriptSig that works with them. 
	     emsg = [],
	     live = true,
             expiration = 0,
	     cid}). %live is a flag. As soon as it is possible that the channel could be closed, we switch the flag to false. We keep trying to close the channel, until it is closed. We don't update the channel state at all.

-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      pubkey = <<>>,
	      bets = 1,%This is a pointer to the merkel tree that stores how many bets you have made in each oracle.
              bets_hash = <<>>}).
-record(oracle, {id, 
		 result, 
		 question, 
		 starts, 
		 type, %0 means order book is empty, 1 means the order book is holding shares of true, 2 means it holds false, 3 means that it holds shares of "bad question". % 3 1
		 orders = 1,
		 orders_hash,
		 creator,
		 %difficulty = 0,
		 done_timer, % 3 4
		 governance = 0,%if it is non-zero, then this is a governance oracle which can update the value of the variables that define the protocol.
		 governance_amount = 0}).
-record(tx_pool, {txs = [],
                  %trees,%this changes once per tx
                  block_trees,%this changes once per block
                  dict = dict:new(), %mirrors trees.
                  facts = [], 
                  height = 0,
		  bytes = 2,
		  checksums = []}).
-record(spend, {from = 0,
	       nonce = 0,
	       fee = 0,
	       to = 0,
	       amount = 0,
	       version = 0}).
-record(create_acc_tx, {from = 0,
                        nonce = 0,
                        fee = 0,
                        pubkey = <<>>,
                        amount = 0}).
-record(multi_tx, {from = 0,
		  nonce = 0,
		  fee = 0,
		  txs = []}).

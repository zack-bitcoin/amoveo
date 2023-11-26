
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
                roots,%LTS  %hash(roots) == prev_block.trees_hash
                meta = <<>>, % we need to calculate locally.
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
-record(sub_acc, {balance = 0,
                 nonce = 0,
                 pubkey,
                 contract_id,
                 type}).
-record(oracle, {id, 
		 result, 
		 question, 
		 starts, %do not make any hard update that would allow this value to be modified. For scalar oracles, it is used to generate the pointers to the rest of the scalar oracle.
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
-record(cs, {from, nonce, fee = 0, 
	     scriptpubkey, scriptsig}).
-record(csc, {from, nonce, fee = 0, 
	      scriptpubkey, scriptsig}).
-record(ctc, {aid1 = 0, aid2 = 0, fee = 0,
	      nonce = 0, id = 0, amount = 0}).
-record(ctc2, {aid1 = 0, aid2 = 0, fee = 0,
               id = 0,
               amount1 = 0, amount2 = 0,
               upper_limit, lower_limit}).
-record(timeout, {aid = 0, nonce = 0, fee = 0, cid = 0, spk_aid1, spk_aid2}).
-record(coinbase, {from = 0, nonce = 0, fee = 0}).
-record(delete_acc_tx, {from = 0,
                        nonce = 0,
                        fee = 0,
                        to = 0}).
-record(ex, {from, nonce = 0, fee = 0, commit = 0}).
-record(nc, {acc1 = 0, acc2 = 0, fee = 0, nonce = 0, 
	     bal1 = 0, bal2 = 0, 
	     delay = 10, id = -1}).
-record(nc_offer, {acc1, nonce, nlocktime, bal1, bal2, miner_commission, %miner commission between 0 and 10 000.
              delay, id, contract_hash}).%this is the anyone can spend trade offer.
-record(nc_accept, 
        {acc2, nc_offer, fee,
         contract_sig}).%this is the tx.
-record(signed, {data="", sig="", sig2=""}).
-record(oracle_close, {from, nonce, fee, oracle_id}).
-record(oracle_new, {from = 0, 
		     nonce = 0, 
		     fee = 0, 
		     question = <<>>, 
		     start, 
		     id, 
		     %recent_price, %if this is a governance oracle, or if it is asking a question, then we need to reference another oracle that closed recently with the state "bad". We reference it so we know the current price of shares.
		     difficulty = 0, 
		     governance, 
		     governance_amount}).

-record(contract_new_tx, {from, contract_hash, fee, many_types, source, source_type}).
-record(contract_use_tx, {from, nonce, fee, contract_id, amount, many, source, source_type}).
-record(sub_spend_tx, {from, nonce, fee, to, amount, contract, type}).

-record(swap_offer, {
          acc1, start_limit, end_limit, salt,
          amount1, cid1, type1, %this is what acc1 gives.
          amount2, cid2, type2, %this is what acc2 gives.
          fee1, %what acc1 pays in fees
          nonce}).
-record(swap_tx, {from, offer, fee}).


-record(contract_evidence_tx,
        {from, nonce, fee,
        contract,%bytecode of contract
        contract_id,
        evidence,%like the script sig from bitcoin.
        prove}
     ).
-record(contract_timeout_tx, {from, nonce, fee, contract_id, proof, contract_hash, row}).%possibly converts it into a new kind of contract. %possibly unsigned
-record(contract_timeout_tx2, {from, nonce, fee, contract_id, proof, contract_hash, row, sink}).%possibly converts it into a new kind of contract. %possibly unsigned
-record(contract_winnings_tx, {from, nonce, fee, contract_id, amount, sub_account, winner, proof, row}).
-record(contract_simplify_tx, {from, nonce, fee, cid, cid2, cid3, m1, m2}).




-record(oracle_winnings, {from, nonce, fee, oracle_id}).
-record(gov, {id, value, lock}).
-record(channel, {id = 0, %the unique id number that identifies this channel
		  acc1 = 0, % a pubkey
		  acc2 = 0, % a different pubkey
		  bal1 = 0, %part of the money initially controlled by acc1.
		  bal2 = 0, %part of the money initially controlled by acc2.
		  amount = 0, %this is how we remember the outcome of the last contract we tested, that way we can undo it.
		  nonce = 1,%How many times has this channel-state been updated. If your partner has a state that was updated more times, then they can use it to replace your final state.
		  last_modified = 0,%this is used to know if a channel_timeout_tx can be called yet. 
		  delay = 0,%this is the minimum of how long you have to wait since "last_modified" to do a channel_timeout_tx. 
                  %every time a channel_slash_tx happens, this delay is updated. This is how long you need to wait before you can do a channel_timeout tx.
		  closed = 0 %when a channel is closed, set this to 1. The channel can no longer be modified, but the VM has access to the state it was closed on. So you can use a different channel to trustlessly pay whoever slashed.
		  %channels closed flag is unused because we delete closed channels.
		  }).
-record(contract, {
      code,
      many_types,
      nonce = 0,
      last_modified = 0,
      delay = 0,
      closed = 0,
      result = <<0:256>>,%if result is an integer in (0,many_types], then all the money goes to that type.
      %otherwise, result can be a merkle root of a tree that describes some other contract with the same source and source_type.
      %or, the result can be the hash of a merkle structure describing how the value is divided up among the participants.
      source = <<0:256>>,
      source_type = 0,
      sink = <<0:256>>,%once a contract finalizes, this records which other contract is recording the combined volume for both contracts.
      volume = 0
}).

-record(matched, {account, oracle, true, false, bad}).
% -record(unmatched, {from, nonce, fee, oracle_id}).
%true, false, and bad are the 3 types of shares that can be purchased from an oracle


-record(market, {id, cid1, type1, amount1, cid2, type2, amount2, shares}).

-record(market_new_tx, {from, nonce = 0, fee, cid1, cid2, type1, type2, amount1, amount2}).
-record(market_liquidity_tx, {from, nonce, fee, mid, amount, cid1, type1, cid2, type2}).
-record(market_swap_tx, {from, nonce, fee, mid, give, take, direction, cid1, type1, cid2, type2}).

-record(trade, {height, value}).%height should be changed to "nonce" eventually.
-record(trade_cancel_tx, {acc, nonce, fee, salt}).

-record(swap_tx2, {from, nonce, fee, offer, match_parts}).

-record(swap_offer2, {
          acc1, start_limit, end_limit,
          cid1, type1, amount1, 
          cid2, type2, amount2,
          salt, start_nonce, parts}).

-record(trees, {accounts, channels, existence,%
		oracles, governance}).%
%we did a hard fork to move the matched and unmatched trees from inside of accounts and oracles to their own tries.
-record(trees2, {accounts, channels, existence,
		oracles, governance, matched,
		unmatched}).

-record(trees3, {accounts, channels, existence,
		oracles, governance, matched,
		unmatched, sub_accounts,
                contracts, trades}).
-record(trees4, {accounts, channels, existence,
		oracles, governance, matched,
		unmatched, sub_accounts,
                contracts, trades, markets}).
-record(trees5, {accounts, channels, existence,
		oracles, governance, matched,
		unmatched, sub_accounts,
                contracts, trades, markets,
                receipts, stablecoins}).

-record(stablecoin, {
          id,
          auction_mode, %can be: false, time_limit, under_coll
          source, %collateral contract id. for the finite stablecoin.
          amount, %amount of collateral locked in the perpetual stablecoin
          code_hash, %hash of code to decide if a collateral smart contract is valid
          timeout, %height at which an timelimit auction should start.
          max_bid_pubkey, %pubkey of whoever made the biggest bid so far.
          max_bid_amount, %how much they had bid.
          timelimit_auction_duration,
          undercollateralization_auction_duration,
          undercollateralization_price_trigger, %out of 100000, if this is 99000, and there are 100 veo worth of perpetual stablecoins, then you can use 99 veo to buy all the finite stablecoins backing the contract. This is like a lower bound on how collaterlized we want the contract to be.
          collateralization_step, %during each undercollateralization auction, how much should we attempt to increase the collateralization by. This should aim for about the midpoint of the range of collateralization we are targeting.
          margin, %margin of current stablecoin contract.
          period %how long until the next finite stablecoin expires. this should be longer than the timelimit_auction_duration

         }).

-record(stablecoin_new_tx, {
     from,
     id,
     fee,
     source,
     source_type,
     code_hash,
     timelimit_auction_duration,
     undercollateralization_auction_duration,
     undercollateralization_price_trigger,
     collateralization_step,
     expiration,%when the first finite stablecoin contract expires. should be about 1 period in the future.
     period,
     margin
}).

-record(exist, {hash, height}).
% -record(oracle_bet, {id, true, false, bad}).%true, false, and bad are the 3 types of shares that can be purchased from an oracle%
-record(orders, {aid, amount, pointer}).%
-record(consensus_state, {empty = true, val, key, unhashed_key, type}).

-record(job, {id, worker, boss, value, salary, balance, time}).
-record(futarchy,
        {fid, %deterministically generated from other values.
         decision_oid, %determines which market gets reverted. true/false
         goal_oid, %determines who wins the bet in the non-reverted market. yes/no
         true_orders = <<0:256>>, %linked list of orders in the order book, by price.
         false_orders = <<0:256>>,
         batch_period,
         last_batch_height = 0,
         liquidity_true = 0, %liquidity in the optional lmsr market.
         liquidity_false = 0,
         shares_true_yes = 0,%total shares purchased for the case where the decision is true, and the goal is yes.
         shares_true_no = 0,
         shares_false_yes = 0,
         shares_false_no = 0,
         active = 1}).
-record(futarchy_unmatched,
        {id,
         owner,%who made this bet
         futarchy_id,
         decision,%the bet doesn't get reverted in which outcome of the decision oracle?
         revert_amount,
         limit_price,
         salt, %256 bits chosen by user. used to generate the id.
         next, %they are in a linked list sorted by price. This is the order book.
         previous %by making it a double linked list, our proofs can be smaller. We don't need to prove the chain back to the futarchy market.
         }).
-record(futarchy_matched,
        {id,
         owner,%who made this bet
         futarchy_id,
         decision,%the bet doesn't get reverted in which outcome of the decision oracle?
         revert_amount,
         win_amount,% > or == the limit_price
         salt 
         }).

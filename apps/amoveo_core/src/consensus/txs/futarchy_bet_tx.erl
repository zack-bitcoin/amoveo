-module(futarchy_bet_tx).
-export([go/4, make_dict/3]).

-include("../../records.hrl").

-record(futarchy_bet_tx,
        {pubkey, nonce, fee,
        fid, %the id of the futarchy market
        limit_price, %the highest price you are willing to pay.
        amount, %the amount of veo you are risking.
        decision, %your bet is not reverted if this decision is selected. true/false
        goal, %you win if the goal oracle finalizes in this state. true/false
        tid %id of the trade that is ahead of you in the order book. This value does _not_ get hashed when signing this transaction, but do hash it when calculating the txid and hashing the block.
        }).

go(_, _, _, _) ->
    ok.
make_dict(_, _, _) ->
    ok.

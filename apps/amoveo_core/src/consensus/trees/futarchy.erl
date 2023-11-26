-module(futarchy).
-export([key_to_int/1, make_id/2, dict_get/2,
         dict_write/2, dict_write/3, dict_get/3
        ]).
%futarchy, {fid, decision_oid, goal_oid, true_orders, false_orders, batch_period, last_batch_height, liquidity_true, shares_true_yes, shares_true_no, liquidity_false, shares_false_yes, shares_false_no, active}
-include("../../records.hrl").


key_to_int(#futarchy{fid = <<X:256>>}) -> X;
key_to_int(<<X:256>>) -> X.
make_id(F = #futarchy{
          decision_oid = <<DOID:256>>,
          goal_oid = <<GOID:256>>,
          batch_period = BP,
          creator = Owner
%          liquidity_true = LT,
%          liquidity_false = LF
         }, _Height) ->
    CoinsSize = 64,
    HeightSize = 32,
    <<OwnerN:264>> = trees2:compress_pub(Owner),
    B = <<DOID:256, GOID:256, BP:HeightSize, 
          OwnerN:264>>,
    ID = hash:doit(B),
    F#futarchy{fid = ID}.

dict_get(ID, Dict, _) ->
    dict_get(ID, Dict).
dict_get(ID, Dict) ->
    case csc:read({?MODULE, ID}, Dict) of
        error -> error;
        {empty, _, _} -> empty;
        {ok, ?MODULE, Val} -> Val
    end.
dict_write(Job, Dict) ->
    dict_write(Job, 0, Dict).
dict_write(Job, _Meta, Dict) ->
    ID = Job#futarchy.fid,
    csc:update({?MODULE, ID}, Job, Dict).

       

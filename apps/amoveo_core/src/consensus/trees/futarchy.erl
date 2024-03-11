-module(futarchy).
-export([key_to_int/1, make_id/3, dict_get/2,
         dict_write/2, dict_write/3, dict_get/3,
         root_hash/1
        ]).
%futarchy, {fid, decision_oid, goal_oid, true_orders, false_orders, liquidity_true, shares_true_yes, shares_true_no, liquidity_false, shares_false_yes, shares_false_no, active, many_trades}
-include("../../records.hrl").


key_to_int(#futarchy{fid = <<X:256>>}) -> X;
key_to_int(<<X:256>>) -> X.
make_id(Owner, Salt, _Height) ->
    %CoinsSize = 64,
    %HeightSize = 32,
    true = is_binary(Salt),
    <<_:256>> = Salt,
    <<OwnerN:264>> = trees2:compress_pub(Owner),
    B = <<%DOID:256, GOID:256, BP:HeightSize, 
          Salt/binary,
          OwnerN:264>>,
    ID = hash:doit(B),
    ID.

root_hash(F = #futarchy{
             true_yes_orders = TYO,
             true_no_orders = TNO,
             false_yes_orders = FYO,
             false_no_orders = FNO,
             shares_true_yes = STY,
             shares_true_no = STN,
             shares_false_yes = SFY,
             shares_false_no = SFN
            }) ->
    B = <<STY:48, STN:48, SFY:48, SFN:48, 
          TYO/binary, TNO/binary,
          FYO/binary, FNO/binary>>,
    H = hash:doit(B),
    F#futarchy{root_hash = H}.

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
    Job2 = root_hash(Job),
    csc:update({?MODULE, ID}, Job2, Dict).

       

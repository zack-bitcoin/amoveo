-module(rng_challenge).
-export([new/1,
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/5, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
         test/0
]).
-define(id, rng_challenge).
-include("../../records.hrl").
%-record(candidate, {sortition_id, layer_number, winner_pubkey, height, next_candidate}).


new(_) ->
    ok.

write(_,_) ->
    ok.

get(_, _) ->
    ok.

delete(_, _) ->
    ok.

dict_update(_, _, _, _, _) ->
    ok.

dict_delete(_, _) ->
    ok.

dict_write(_, _) ->
    ok.

dict_get(_, _) ->
    ok.

verify_proof(_, _, _, _) ->
    ok.

make_leaf(_, _, _) ->
    ok.

key_to_int(_) ->
    ok.

deserialize(_) ->
    ok.

serialize(_) ->
    ok.

all() ->
    ok.

test() ->
    ok.

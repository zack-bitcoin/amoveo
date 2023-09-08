-module(jobs).
-export([key_to_int/1, make_id/2, dict_get/2]).

-include("../../records.hrl").

key_to_int(#job{id = <<X:256>>}) ->
    X.

make_id(<<Worker:264>>, <<Salt:256>>) ->
    hash:doit(<<Worker:264, Salt:256>>).
dict_get(ID, Dict) ->
    case csc:read({jobs, ID}, Dict) of
        error -> error;
        {empty, _, _} -> empty;
        {ok, jobs, Val} -> Val
    end.
             

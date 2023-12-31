-module(futarchy_matched).
-export([key_to_int/1, dict_get/2,
         dict_write/2, dict_write/3, dict_get/3
        ]).

-include("../../records.hrl").
key_to_int(#futarchy_matched{id = <<X:256>>}) -> X;
key_to_int(<<X:256>>) -> X.
%make_id(Pub, Salt) ->
%    <<_:256>> = Salt,
%    Pub2 = case size(Pub) of
%               33 -> Pub;
%               65 -> trees2:compress_pub(Pub)
%           end,
%    B = <<0, Pub2/binary, Salt/binary>>,
%    hash:doit(B).
    
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
    ID = Job#futarchy_matched.futarchy_id,
    csc:update({?MODULE, ID}, Job, Dict).

       

-module(new_contract_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").
make_dict(From, CH, Types, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce,
    #new_contract_tx{from = From, nonce = Nonce, fee = Fee, contract_hash = CH, many_types = Types}.

go(Tx, Dict, NewHeight, _) ->
    #new_contract_tx{
      from = From,
      nonce = Nonce,
      fee = Fee,
      contract_hash = CH,
      many_types = MT} = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    NC = contracts:new(CH, MT),
    contracts:dict_write(NC, Dict2).
    
    

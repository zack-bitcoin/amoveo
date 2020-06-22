-module(contract_timeout_tx).
-export([go/4, make_dict/3]).
-include("../../records.hrl").

make_dict(From, ContractID, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    #contract_timeout_tx{from = From, nonce = Nonce, fee = Fee, contract_id = ContractID}.

go(Tx, Dict, NewHeight, _) ->
    %resolves the contract, or possibly converts it into a different kind of contract.
    %last_modified + delay > nowHeight
    %update contract to be closed and resolved
    #contract_timeout_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    contract_id = CID
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    Contract = contracts:dict_get(CID, Dict2),
    #contract{
    last_modified = LM,
    delay = Delay,
    closed = 0
   } = Contract,
    true = (LM + Delay) < NewHeight,
    Contract2 = 
        Contract#contract{
          closed = 1
         },
    contracts:dict_write(Contract2, Dict2).

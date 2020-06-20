-module(use_contract_tx).
-export([go/4, make_dict/4, from/1, cid/1]).
-include("../../records.hrl").

%this allows you to buy all types in a subcurry together.

from(Tx) -> Tx#use_contract_tx.from.
cid(Tx) -> Tx#use_contract_tx.contract_id.

make_dict(From, CID, Amount, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    C = trees:get(contracts, CID),
    Many = contracts:many_types(C),
    #use_contract_tx{from = From, nonce = Nonce, fee = Fee, contract_id = CID, amount = Amount, many = Many}.
go(Tx, Dict, NewHeight, _) ->
    #use_contract_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    contract_id = CID,
    amount = Amount,
    many = Many
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee-Amount, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    Contract = contracts:dict_get(CID, Dict2),
    #contract{
      many_types = Many,
      nonce = ContractNonce,
      last_modified = LM,
      delay = Delay,
      closed = Closed,
      result = Result,
      source = Source,
      volume = Volume
     } = Contract,
    %we need to update source accounts, and subcurrency accounts.
    send_sub_accounts(Many, From, CID, Amount, Dict2).
send_sub_accounts(0, _, _, _, Dict) ->
    Dict;
send_sub_accounts(N, From, CID, Amount, Dict) ->
    Key = sub_accounts:make_key(From, CID, N),
    OA = sub_accounts:dict_get(Key, Dict),
    A2 = 
        case OA of
            empty -> sub_accounts:new(From, Amount, CID, N);
            _ -> sub_accounts:dict_update(Key, Dict, Amount, none)
        end,
    Dict2 = sub_accounts:dict_write(A2, Dict),
    send_sub_accounts(N - 1, From, CID, Amount, Dict2).

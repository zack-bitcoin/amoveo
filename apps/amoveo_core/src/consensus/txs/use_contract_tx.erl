-module(use_contract_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").

%this allows you to buy all types in a subcurry together.

make_dict(From, Contract, Amount, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce,
    #use_contract_tx{from = From, nonce = Nonce, fee = Fee, contract_hash = Contract, amount = Amount}.
go(Tx, Dict, NewHeight, _) ->
    #use_contract_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    contract_hash = CID,
    many = Many,
    amount = Amount
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee-Amount, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    %CID = contracts:make_id({Code, Many}),
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
    Dict2 = sub_accounts:dict_update(Key, Dict, Amount),
    send_sub_accounts(N - 1, From, CID, Amount, Dict2).

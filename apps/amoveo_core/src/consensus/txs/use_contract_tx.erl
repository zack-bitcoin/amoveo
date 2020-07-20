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
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
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
      source_type = SourceType,
      volume = Volume
     } = Contract,
    Volume2 = Volume + Amount,
    true = Volume2 > 0,
    %we need to update source accounts, and subcurrency accounts.
    Contract2 = Contract#contract{
                  volume = Volume2
                 },
    Dict3 = contracts:dict_write(Contract2, Dict2),
    Dict4 = 
        case Source of
            <<0:256>> ->%veo type
                Facc2 = accounts:dict_update(From, Dict3, -Amount, none),
                accounts:dict_write(Facc2, Dict3);%using veo to buy a complete set of subcurrencies
            _ ->
                Key = sub_accounts:make_key(From, Source, SourceType),
                OA = sub_accounts:dict_get(Key, Dict3),
                A2 = 
                    case OA of
                        empty ->
                            true = Amount < 0,
                            sub_accounts:new(From, -Amount, Source, SourceType);%getting paid in a source subcurrency in exchange for providing a complete set of sub-subcurrencies.
                        _ ->
                            sub_accounts:dict_update(Key, Dict3, -Amount, none)%buying a complete set of sub-subcurrencies using your source subcurrency.
                    end,
                sub_accounts:dict_write(A2, Dict3)
        end,
    send_sub_accounts(Many, From, CID, Amount, Dict4).
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

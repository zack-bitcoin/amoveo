-module(contract_use_tx).
-export([go/4, make_dict/4, make_dict/7, from/1, cid/1]).
-include("../../records.hrl").

%this allows you to buy all types in a subcurrency together.

from(Tx) -> Tx#contract_use_tx.from.
cid(Tx) -> Tx#contract_use_tx.contract_id.

make_dict(From, CID, Amount, Fee, Many, Source, SourceType) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    #contract_use_tx{from = From, nonce = Nonce, fee = Fee, contract_id = CID, amount = Amount, many = Many, source = Source, source_type = SourceType}.
make_dict(From, CID, Amount, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    C = trees:get(contracts, CID),
    #contract{
               many_types = Many,
               source = Source,
               source_type = SourceType
             } = C,
    #contract_use_tx{from = From, nonce = Nonce, fee = Fee, contract_id = CID, amount = Amount, many = Many, source = Source, source_type = SourceType}.
go(Tx, Dict, NewHeight, NonceCheck) ->
    #contract_use_tx{
    from = From,
    nonce = Nonce0,
    fee = Fee,
    contract_id = CID,
    amount = Amount,
    many = Many,
    source = Source,
    source_type = SourceType
   } = Tx,
    true = NewHeight > forks:get(32),
%    Nonce = if
%		NonceCheck -> Nonce0;
%		true -> none
%	    end,
    Nonce = nonce_check:doit(
              NonceCheck, 
              Tx#contract_use_tx.nonce),
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    Contract = contracts:dict_get(CID, Dict2),
    #contract{
      many_types = Many,
               %nonce = ContractNonce,
      last_modified = LM,
      delay = Delay,
      closed = Closed,
      result = Result,
      source = Source,
      source_type = SourceType,
      volume = Volume
     } = Contract,
    Volume2 = Volume + Amount,
    true = Volume2 >= 0,
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
                F52 = forks:get(52),
                Key = if
                          NewHeight < F52 ->
                              sub_accounts:make_key(From, Source, SourceType);
                          true ->
                              sub_accounts:make_v_key(From, Source, SourceType)
                      end,
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

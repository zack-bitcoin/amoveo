-module(channel2contract_tx).
-export([go/4, make_dict/4]).

-include("../../records.hrl").

-record(channel2contract_tx, 
        {from, nonce, fee, accounts, channel_id}).

make_dict(From, Accounts, CID, Fee) ->
    Acc = trees:get(accounts, From),
    Nonce = Acc#acc.nonce + 1,
    #channel2contract_tx{
           from = From,
           nonce = Nonce,
           accounts = Accounts,
           fee = Fee,
           channel_id = CID
          }.

go(Tx, Dict, NewHeight, _) ->
    #channel2contract_tx{
    from = From,
    nonce = Nonce,
    accounts = Accounts,
    fee = Fee,
    channel_id = CID
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),

    Channel = sub_channels:dict_get(CID, Dict2),
    #sub_channel{
                  closed = 0,
                  accounts = AH,
                  contract_hash = CH,
                  contract_id = Source,
                  type = SourceType,
                  amount = Amount
                  } = Channel,
    true = is_in(From, Accounts),
    AH = hash:doit(Accounts),

    %change the channel into a contract.
    SC2 = Channel#sub_channel{closed = 1},
    Dict3 = sub_channels:dict_write(SC2, Dict2),
   
    ContractID = contracts:make_id(CH, length(Accounts), Source, SourceType),
    {Contract, Dict4} = 
        case contracts:dict_get(ContractID) of
            empty ->
                X = contracts:new(CH, length(Accounts), Source, SourceType),
                {X, contracts:dict_write(X, Dict3)};
            X -> {X, Dict3}
        end,
    send_subcurrencies(Accounts, Amount, ContractID, 1, Dict4).

is_in(_, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> 
    is_in(X, T).

send_subcurrencies([], _, _, _, Dict) ->
    Dict;
send_subcurrencies([A|T], Amount, CID, N, Dict) ->
    ToKey = sub_accounts:make_key(A, CID, N),
    SA = case sub_accounts:dict_get(ToKey, Dict) of
             empty ->
                 sub_accounts:new(A, Amount, CID, N);
             X ->
                 B = X#sub_acc.balance,
                 X#sub_acc{
                   balance = B + Amount
                   }
         end,
    Dict2 = sub_accounts:dict_write(SA, Dict),
    send_subcurrencies(T, Amount, CID, N+1, Dict2).
            

-module(contract_winnings_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").
%-record(contract_winnings_tx, {from, nonce, fee, contract_id, amount}).

make_dict(From, SubAcc, CID, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    SA = trees:get(sub_accounts, SubAcc),
    Amount = SA#sub_acc.balance,
    #contract_winnings_tx{from = From, winner = From, sub_account = SubAcc, nonce = Nonce, contract_id = CID, fee = Fee, amount = Amount}.

go(Tx, Dict, NewHeight, _) ->
    #contract_winnings_tx{
    from = From,
    winner = Winner,
    nonce = Nonce,
    contract_id = CID,
    sub_account = SubAcc,
    fee = Fee,
    amount = Amount
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),

    SA = sub_accounts:dict_get(SubAcc, Dict2),
    #sub_acc{
              balance = Amount,
              type = Type,
              contract_id = CID,
              pubkey = Winner
            } = SA,
    Dict3 = sub_accounts:dict_delete(SubAcc, Dict2),
    
    Contract = contracts:dict_get(CID, Dict3),
    #contract{
               closed = Closed,
               result = Result
             } = Contract,
    false = (Closed == 0),
   
    %use Type to look into Result to see if we won.
    case Result of
        <<Type:256>> ->
            %win it all.
            Wacc = accounts:dict_update(Winner, Dict3, Amount, none),
            accounts:dict_write(Wacc, Dict3);
        <<_:256>> ->
            %get nothing.
            Dict3
    end.
    

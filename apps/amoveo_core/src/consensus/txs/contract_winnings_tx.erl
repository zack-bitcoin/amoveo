-module(contract_winnings_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").
%-record(contract_winnings_tx, {from, nonce, fee, contract_id, amount}).

make_dict(From, SubAcc, CID, Fee) ->
    make_dict(From, SubAcc, CID, Fee, 0).
make_dict(From, SubAcc, CID, Fee, Proof) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    SA = trees:get(sub_accounts, SubAcc),
    Amount = SA#sub_acc.balance,
    #contract_winnings_tx{from = From, winner = From, sub_account = SubAcc, nonce = Nonce, contract_id = CID, fee = Fee, amount = Amount, proof = Proof}.

go(Tx, Dict, NewHeight, _) ->
    #contract_winnings_tx{
    from = From,
    winner = Winner,
    nonce = Nonce,
    contract_id = CID,
    sub_account = SubAcc,
    fee = Fee,
    amount = Amount,
    proof = Proof
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
               source = Source,
               source_type = SourceType,
               closed = Closed,
               volume = V1,
               result = Result
             } = Contract,
    false = (Closed == 0),
   
    io:fwrite("contract winings tx result source\n"),
    <<Type2:256>> = Result,
    io:fwrite(packer:pack({Type, Type2})),
    io:fwrite("\n"),
    %use Type to look into Result to see if we won.
    case {Result, Source} of
        {<<Type:256>>, <<0:256>>} ->
            %win it all as veo.
            Wacc = accounts:dict_update(Winner, Dict3, Amount, none),
            Dict4 = accounts:dict_write(Wacc, Dict3),
            Contract2 = Contract#contract{
                          volume = V1 - Amount
                         },
            contracts:dict_write(Contract2, Dict4);
        {<<Type:256>>, <<CID2:256>>} ->
            %win it all as a different subcurrency.
            Key = sub_accounts:make_key(Winner, CID2, SourceType),
            OA = sub_accounts:dict_get(Key, Dict3),
            A2 = case OA of
                     empty ->
                         sub_accounts:new(From, Amount, CID2, SourceType);
                     _ ->
                         sub_accounts:dict_update(Key, Dict3, Amount, none)
                 end,
            sub_accounts:dict_write(A2, Dict3);
        {<<MRoot:256>>, Source} ->
            case Proof of
                {{<<MRoot:256>>, Row, Proof2},
                 {_, CH2, Proof3}}->
                    %it is a matrix
                    MT = mtree:new_empty(5, 32, 0),
                    CFG = mtree:cfg(MT),
                    true = verify:proof(<<MRoot:256>>, Row, Proof2, CFG),
                    true = verify:proof(<<MRoot:256>>, CH2, Proof3, CFG),
                    CID2 = contracts:make_id(CH2, length(Row), Source, SourceType),
                    RContract = contracts:dict_get(CID2, Dict3),
                    {RContract1, Dict4} = 
                        case RContract of
                            empty ->
                                C = contracts:new(CH2, length(Row), Source, SourceType),
                                {C, contracts:dict_write(C, Dict3)};
                            _ -> {RContract, Dict3}
                        end,
                    RowSum = lists:foldl(fun(A, B) -> A + B end, 0, Row),
                    CID2 = contracts:make_id(RContract1),
                    payout_row(Winner, CID2, Row, Dict4, 1);
                PayoutVector ->
                    io:fwrite("contract winnings: payout vector"),
                    io:fwrite(packer:pack(PayoutVector)),
                    io:fwrite("\n"),
                    <<MRoot:256>> = hash:doit(PayoutVector),
                    %TODO
                    %payout in the source currency.
                    1=2,
                    ok
            end;
        {<<_:256>>, _} ->
            %get nothing.
            %sub_account is deleted.
            1=2,
            Dict3
    end.
    

payout_row(_, _, [], Dict, _) -> Dict;
payout_row(Winner, CID, Row, Dict, N) ->
    ToKey = sub_accounts:make_key(Winner, CID, N),
    A = hd(Row),
    Acc = sub_accounts:dict_update(ToKey, Dict, A, none),
    Dict2 = sub_accounts:dict_write(Acc, Dict),
    payout_row(Winner, CID, tl(Row), Dict2, N+1).

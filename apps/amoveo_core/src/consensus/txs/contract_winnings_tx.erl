-module(contract_winnings_tx).
-export([go/4, make_dict/4, make_dict/5]).
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
               result = Result,
               sink = SinkCID
             } = Contract,
    false = (Closed == 0),
   
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
            Dict4 = sub_accounts:dict_write(A2, Dict3),
            Contract2 = Contract#contract{
                          volume = V1 - Amount
                         },
            contracts:dict_write(Contract2, Dict4);
        {<<MRoot:256>>, Source} ->
            case Proof of
                {{Row, CH2},%Row, 
                 {<<MRoot:256>>, RowHash, Proof2},
                 _}->%{<<MRoot:256>>, _CID2, Proof3}}->
                    %it is a matrix
                    MT = mtree:new_empty(5, 32, 0),
                    CFG = mtree:cfg(MT),
                    %CH2Leaf = leaf:new(0, CID2, 0, CFG),
                    RowLeaf = leaf:new(1, RowHash, 0, CFG),
                    true = verify:proof(<<MRoot:256>>, RowLeaf, Proof2, CFG),
                    %true = verify:proof(<<MRoot:256>>, CH2Leaf, Proof3, CFG),
                    
                    
%                    _RContract = 
%                        case RContract0 of
%                            empty ->
%                                CID2 = contracts:make_id(CH2, length(Row), Source, SourceType),%to verify CH2
%                                RMany = length(Row),
%                                contracts:new(CH2, RMany, Source, SourceType);
%                            X -> X
%                        end,
                    payout_row(Winner, SinkCID, Row, Dict3, 1, Amount);
                PayoutVector when is_list(PayoutVector) ->
                    RContract = 
                        case SinkCID of
                            <<0:256>> -> Contract;
                            _ -> contracts:dict_get(SinkCID, Dict3)
                        end,
                    <<MRoot:256>> = hash:doit(resolve_contract_tx:serialize_row(PayoutVector, <<>>)),
                    <<A:32>> = lists:nth(Type, PayoutVector),
                    <<Max:32>> = <<-1:32>>,
                    Amount2 = Amount * A div Max,
                    case Source of
                        <<0:256>> ->%payout to veo
                            Wacc = accounts:dict_update(Winner, Dict3, Amount2, none),
                            Dict4 = accounts:dict_write(Wacc, Dict3),
                            Contract2 = RContract#contract{
                                          volume = RContract#contract.volume - Amount2
                                         },
                            contracts:dict_write(Contract2, Dict4);
                        <<CID3:256>> ->%payout to subcurrency
                            Key = sub_accounts:make_key(Winner, CID3, SourceType),
                            OA = sub_accounts:dict_get(Key, Dict3),
                            A2 = case OA of
                                     empty ->
                                         sub_accounts:new(From, Amount2, CID3, SourceType);
                                     _ ->
                                         sub_accounts:dict_update(Key, Dict3, Amount2, none)
                                 end,
                            Dict4 = sub_accounts:dict_write(A2, Dict3),
                            Contract2 = RContract#contract{
                                          volume = RContract#contract.volume - Amount2
                                         },
                            contracts:dict_write(Contract2, Dict4)
                        end
            end;
        {<<_:256>>, _} ->
            1=2
    end.
    

payout_row(_, _, [], Dict, _, _) -> Dict;
payout_row(Winner, CID, Row, Dict, N, Amount) ->
    ToKey = sub_accounts:make_key(Winner, CID, N),
    <<A0:32>> = hd(Row),
    <<Max:32>> = <<-1:32>>,
    A = A0 * Amount div Max,
    Dict2 = 
        if
            A == 0 -> %if you receive 0, then don't change anything.
                Dict;
            A > 0 ->
                Acc = 
                    case sub_accounts:dict_get(ToKey, Dict) of
                        empty -> %if the acccount doesn't exist, create it.
                            sub_accounts:new(Winner, A, CID, N);
                        X -> X#sub_acc{
                               balance = X#sub_acc.balance + A
                              }
                    end,
                sub_accounts:dict_write(Acc, Dict)
        end,
    payout_row(Winner, CID, tl(Row), Dict2, N+1, Amount).

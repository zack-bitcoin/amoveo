-module(contract_winnings_tx).
-export([go/4, make_dict/5, make_dict/6]).
-include("../../records.hrl").

make_dict(From, SubAcc, CID, Fee, PayoutVector) ->
    make_dict(From, SubAcc, CID, Fee, 0, PayoutVector).
make_dict(From, SubAcc, CID, Fee, Row, Proof) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    SA = trees:get(sub_accounts, SubAcc),
    Amount = SA#sub_acc.balance,
    #contract_winnings_tx{from = From, winner = From, sub_account = SubAcc, nonce = Nonce, contract_id = CID, fee = Fee, amount = Amount, proof = Proof, row = Row}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #contract_winnings_tx{
    from = From,
    winner = Winner,
    nonce = Nonce0,
    contract_id = CID,
    sub_account = SubAcc,
    fee = Fee,
    amount = Amount,
    row = Row,
    proof = Proof
   } = Tx,
    Nonce = if
		NonceCheck -> Nonce0;
		true -> none
	    end,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),

    SA = sub_accounts:dict_get(SubAcc, Dict2),
    #sub_acc{
              balance = Amount,%
              type = Type,
              contract_id = CID,%
              pubkey = Winner
            } = SA,
    true = Amount > 0,
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
  
    <<MRoot:256>> = Result,
    case Proof of
         {<<MRoot:256>>, RowHash, Proof2} ->
          %pay as subcurrencies in a different contract.
            MT = mtree:new_empty(5, 32, 0),
            CFG = mtree:cfg(MT),
            RowLeaf = leaf:new(Type, RowHash, 0, CFG),
            RowHash = hash:doit(contract_evidence_tx:serialize_row(Row, <<>>)),
            true = verify:proof(<<MRoot:256>>, RowLeaf, Proof2, CFG),
            RContract = contracts:dict_get(SinkCID, Dict3),
            #contract{
                       closed = 0%this prevents anyone from moving money into another resolved contract. So they are forced to do the matrix multiplication simplification when possible.
                     } = RContract,
            payout_row(Winner, SinkCID, Row, Dict3, 1, Amount);

        PayoutVector when is_list(PayoutVector) ->
                                                %it is a vector
            RContract = 
                case SinkCID of
                    <<0:256>> -> Contract;
                    _ -> contracts:dict_get(SinkCID, Dict3)
                end,
            <<MRoot:256>> = hash:doit(contract_evidence_tx:serialize_row(PayoutVector, <<>>)),
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
                    Key = sub_accounts:make_key(Winner, Source, SourceType),
                    OA = sub_accounts:dict_get(Key, Dict3),
                    A2 = case OA of
                             empty ->
                                 sub_accounts:new(From, Amount2, Source, SourceType);
                             _ ->
                                 sub_accounts:dict_update(Key, Dict3, Amount2, none)
                         end,
                    Dict4 = sub_accounts:dict_write(A2, Dict3),
                    Contract2 = RContract#contract{
                                  volume = RContract#contract.volume - Amount2
                                 },
                    contracts:dict_write(Contract2, Dict4)
            end
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
                        X -> 
                            B = X#sub_acc.balance,
                            X#sub_acc{
                               balance = B + A
                              }
                    end,
                sub_accounts:dict_write(Acc, Dict)
        end,
    payout_row(Winner, CID, tl(Row), Dict2, N+1, Amount).

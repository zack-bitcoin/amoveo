-module(pair_buy_tx).
-export([go/4, make_offer/12, make_dict/2,
        fee_helper/3]).
-include("../../records.hrl").


make_dict(From, SPBOffer) ->
    PBOffer = testnet_sign:data(SPBOffer),
    #pair_buy_offer{
           fee1 = Fee1,
           fee2 = Fee2} = PBOffer,
    #pair_buy_tx{from = From, offer = SPBOffer, 
             fee = Fee1 + Fee2}.

make_offer(From, StartLimit, EndLimit, SourceCID, SourceType, Amount1, Fee1, Amount2, Fee2, Subs1, Subs2, CH) ->

    Nonce = 
        case SourceCID of
            <<0:256>> -> 
                A = trees:get(accounts, From),
                A#acc.nonce + 1;
            _ ->
                Key1 = sub_accounts:make_key(From, SourceCID, SourceType),
                A = trees:get(sub_accounts, Key1),
                A#sub_acc.nonce + 1
        end,
    MT = length(Subs1),
    NewCID = contracts:make_id(CH, MT, SourceCID, SourceType),
    #pair_buy_offer{
                     acc1 = From,
                     nonce = Nonce,
                     contract_hash = CH,
                     start_limit = StartLimit,
                     end_limit = EndLimit,
                     source_id = SourceCID,
                     source_type = SourceType,
                     new_id = NewCID,
                     amount1 = Amount1, 
                     amount2 = Amount2,
                     subs1 = Subs1,
                     subs2 = Subs2,
                     fee1 = Fee1,
                     fee2 = Fee2
                   }.
go(Tx, Dict, NewHeight, _) ->
    #pair_buy_tx{
    from = Acc2,
    offer = SPBO,
    fee = Fee
   } = Tx,
    true = testnet_sign:verify(SPBO),
    PBO = testnet_sign:data(SPBO),
    #pair_buy_offer{
                     fee1 = Fee1,
                     fee2 = Fee2,
                     acc1 = Acc1,
                     nonce = Nonce,
                     start_limit = SL,
                     end_limit = EL,
                     source_id = SourceCID,
                     source_type = SourceType,
                     contract_hash = CH,
                     new_id = NewCID,
                     amount1 = Amount1,%amount of veo/source currency gained/lost by acc1
                     amount2 = Amount2,
                     subs1 = Subs1,%subcurrencies received/lost by acc1
                     subs2 = Subs2
                   } = PBO,
    Fee = Fee1 + Fee2,
    true = NewHeight >= SL,
    true = NewHeight =< EL,
    TwoE32 = 4294967295,%(2**32 - 1) highest expressible value in chalang integers. payout quantities need to sum to this.
    true = contract_evidence_tx:column_sum(TwoE32, [Subs1, Subs2]),
    true = correct_vector_format(Subs1),
    true = correct_vector_format(Subs2),

%we want Acc1 to be able to cancel his offer by making some unrelated tx to increase his nonce.
    Dict1 = 
        case SourceCID of
            <<0:256>> ->
                A1 = accounts:dict_get(Acc1, Dict),
                A1N = A1#acc.nonce,
                true = A1N < Nonce,
                A1_2 = A1#acc{
                         nonce = A1N + 1
                        },
                accounts:dict_write(A1_2, Dict);
        _ ->
                Key1 = sub_accounts:make_key(Acc1, SourceCID, SourceType),
                A1 = sub_accounts:dict_get(Key1, Dict),
                A1N = A1#sub_acc.nonce,
                true = A1N < Nonce,
                A1_2 = A1#sub_acc{
                         nonce = A1N + 1
                        },
                sub_accounts:dict_write(A1_2, Dict)
        end,
    Dict2 = fee_helper(Fee1, Acc1, Dict1),
    Dict3 = fee_helper(Fee2, Acc2, Dict2),
    Dict4 = change_sub(-Amount1, Acc1, SourceCID, SourceType, Dict3),%take amount1 away
    Dict5 = change_sub(-Amount2, Acc2, SourceCID, SourceType, Dict4),
    MT = length(Subs1),
    MT = length(Subs2),
    NewCID = contracts:make_id(CH, MT, SourceCID, SourceType),
    %Amount is how much subcurrency to pay out.
    Amount = Amount1 + Amount2,
    C1 = case contracts:dict_get(NewCID, Dict5) of
             empty ->
                 true = Amount >= 0,
                 contracts:new(CH, MT, SourceCID, SourceType);
             X -> 
                 true = ((X#contract.volume + Amount) >= 0),
                 X
         end,
    C2 = C1#contract{
           volume = Amount + C1#contract.volume
          },
    %payout subcurrencies to acc1 amount * sub1
    Dict6 = send_sub_accounts(1, Acc1, NewCID, Amount, Subs1, Dict5),
    Dict7 = send_sub_accounts(1, Acc2, NewCID, Amount, Subs2, Dict6),

    contracts:dict_write(C2, Dict7).

send_sub_accounts(_, _, _, _, [], Dict) ->
    Dict;
send_sub_accounts(N, From, CID, Amount0, [<<0:32>>|ST], Dict) ->
    send_sub_accounts(N+1, From, CID, Amount0, ST, Dict);
send_sub_accounts(N, From, CID, Amount0, [<<Scale:32>>|ST], Dict) ->
    Key = sub_accounts:make_key(From, CID, N),
    OA = sub_accounts:dict_get(Key, Dict),
    Max = 4294967295,%(2**32 - 1) highest expressible value in chalang integers. payout quantities need to sum to this.
    Amount = Amount0 * Scale div Max,
    A2 = 
        case OA of
            empty -> sub_accounts:new(From, Amount, CID, N);
            _ -> sub_accounts:dict_update(Key, Dict, Amount, none)
        end,
    Dict2 = sub_accounts:dict_write(A2, Dict),
    send_sub_accounts(N-1, From, CID, Amount0, ST, Dict2).


fee_helper(Fee, Add, Dict) ->
    if
        Fee == 0 -> Dict;
        Fee > 0 ->
            A = accounts:dict_update(Add, Dict, -Fee, none),
            accounts:dict_write(A, Dict)
    end.
change_sub(Amount, Acc, Source, SourceType, Dict) ->
    if
        Amount == 0 -> Dict;
        Source == <<0:256>> ->
            A = accounts:dict_update(Acc, Dict, Amount, none),
            accounts:dict_write(A, Dict);
        true ->
            Key = sub_accounts:make_key(Acc, Source, SourceType),
            A = sub_accounts:dict_update(Key, Dict, Amount, none),
            sub_accounts:dict_write(A, Dict)
    end.

correct_vector_format([]) -> true;
correct_vector_format([<<_:32>>|T]) ->
    correct_vector_format(T);
correct_vector_format([X|T]) when (X < 0) ->
    false.

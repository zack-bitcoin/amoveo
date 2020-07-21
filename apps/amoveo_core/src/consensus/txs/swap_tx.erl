-module(swap_tx).
-export([go/4, make_offer/11, make_dict/2]).
-include("../../records.hrl").

make_dict(From, SNCOffer) ->
    NCOffer = testnet_sign:data(SNCOffer),
    #swap_offer{
           fee1 = Fee1,
           fee2 = Fee2} = NCOffer,
    #swap_tx{from = From, offer = SNCOffer, 
             fee = Fee1 + Fee2}.

make_offer(From, StartLimit, EndLimit, CID1, Type1, Amount1, CID2, Type2, Amount2, Fee1, Fee2) ->
   

    Nonce = 
        case CID1 of
            <<0:256>> -> 
                A = trees:get(accounts, From),
                A#acc.nonce + 1;
            _ ->
                Key1 = sub_accounts:make_key(From, CID1, Type1),
                A = trees:get(sub_accounts, Key1),
                A#sub_acc.nonce + 1
        end,
    #swap_offer{
           acc1 = From,
            nonce = Nonce,
            start_limit = StartLimit,
            end_limit = EndLimit,
            cid1 = CID1,
            type1 = Type1,
            amount1 = Amount1, 
            cid2 = CID2,
            type2 = Type2,
            amount2 = Amount2,
            fee1 = Fee1,
            fee2 = Fee2
           }.
go(Tx, Dict, NewHeight, _) ->
    #swap_tx{
    from = Acc2,
    offer = SNCO
   } = Tx,
    true = testnet_sign:verify(SNCO),
    NCO = testnet_sign:data(SNCO),
    #swap_offer{
         fee1 = Fee1,
         fee2 = Fee2,
         acc1 = Acc1,
         nonce = Nonce,
         start_limit = SL,
         end_limit = EL,
         cid1 = CID1,
         type1 = Type1,
         amount1 = Amount1,
         cid2 = CID2,
         type2 = Type2,
         amount2 = Amount2
        } = NCO,
    true = NewHeight >= SL,
    true = NewHeight =< EL,

%we want Acc1 to be able to cancel his offer by making some unrelated tx to increase his nonce.
    case CID1 of
        <<0:256>> ->
            A1 = accounts:dict_get(Acc1, Dict),
            true = A1#acc.nonce == (Nonce - 1);
        _ ->
            Key1 = sub_accounts:make_key(Acc1, CID1, Type1),
            A1 = sub_accounts:dict_get(Key1, Dict),
            true = A1#sub_acc.nonce == (Nonce - 1)
        end,

    Dict2 = fee_helper(Fee1, Acc1, Dict),
    Dict3 = fee_helper(Fee2, Acc2, Dict2),
    Dict4 = move_helper(Acc1, Acc2, Amount1, CID1, Type1, Dict3),
    Dict5 = move_helper(Acc2, Acc1, Amount2, CID2, Type2, Dict4),
    Dict5.

fee_helper(Fee, Add, Dict) ->
    if
        Fee == 0 -> Dict;
        Fee > 0 ->
            A = accounts:dict_update(Add, Dict, -Fee, none),
            accounts:dict_write(A, Dict)
    end.

move_helper(Acc1, Acc2, Amount, CID, Type, Dict) ->
    %from acc1 to acc2
    case {CID, Type} of
        {<<0:256>>, 0} ->
            move_veo(Acc1, Acc2, Amount, Dict);
        _ ->
            move_sub(Acc1, Acc2, Amount, CID, Type, Dict)
    end.
        
    
move_veo(Acc1, Acc2, Amount, Dict) ->
    %from Acc1 to Acc2
    A1 = accounts:dict_update(Acc1, Dict, -Amount, none),
    DictA = accounts:dict_write(A1, Dict),
    A2 = accounts:dict_update(Acc2, DictA, Amount, none),
    accounts:dict_write(A2, DictA).

move_sub(Acc1, Acc2, Amount, CID, Type, Dict) ->
    %moves subcurrency from Acc1 to Acc2
    Key1 = sub_accounts:make_key(Acc1, CID, Type),
    SA1 = sub_accounts:dict_update(Key1, Dict, -Amount, none),
    DictA = sub_accounts:dict_write(SA1, Dict),
    Key2 = sub_accounts:make_key(Acc2, CID, Type),
    SA2 = sub_accounts:dict_get(Key2, DictA),
    SA2B = case SA2 of
               empty -> sub_accounts:new(Acc2, Amount, CID, Type);
               _ -> sub_accounts:dict_update(Key2, DictA, Amount, none)
           end,
    sub_accounts:dict_write(SA2B, DictA).
    
    

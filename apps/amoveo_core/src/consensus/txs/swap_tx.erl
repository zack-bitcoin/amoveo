-module(swap_tx).
-export([go/4, make_offer/10, make_dict/3,
        fee_helper/3, trade_id_maker/2]).
-include("../../records.hrl").

make_dict(From, SNCOffer, Fee) ->
    NCOffer = signing:data(SNCOffer),
    #swap_offer{
           fee1 = Fee1
               } = NCOffer,
    #swap_tx{from = From, offer = SNCOffer, 
             fee = Fee - Fee1}.

make_offer(From, StartLimit, EndLimit, 
           CID1, Type1, Amount1, 
           CID2, Type2, Amount2, 
           Fee1) ->
    Nonce = 
        case CID1 of
            <<0:256>> -> 
                A = trees:get(accounts, From),
                A#acc.nonce + 1;
            _ ->
                Key1 = sub_accounts:make_key(From, CID1, Type1),
                case trees:get(sub_accounts, Key1) of
                    empty -> 1;
                    A ->
                        A#sub_acc.nonce + 1
                end
        end,
    Salt = crypto:strong_rand_bytes(32),
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
                 salt = Salt
           }.
go(Tx, Dict0, NewHeight, _) ->
    true = NewHeight > forks:get(32),
    #swap_tx{
    from = Acc2,
    offer = SNCO,
    fee = Fee
   } = Tx,
    true = signing:verify(SNCO),
    NCO = signing:data(SNCO),
    #swap_offer{
                 fee1 = Fee1,
                 acc1 = Acc1,
                 nonce = Nonce,
                 start_limit = SL,
                 end_limit = EL,
                 cid1 = CID1,
                 type1 = Type1,
                 amount1 = Amount1,
                 cid2 = CID2,
                 type2 = Type2,
                 amount2 = Amount2,
                 salt = Salt
               } = NCO,
%    Fee = Fee1 + Fee2,
    true = NewHeight >= SL,
    true = NewHeight =< EL,

%we want Acc1 to be able to cancel his offer by making some unrelated tx to increase his nonce.
%    Dict1 = 
    TID = trade_id_maker(Acc1, Salt),
    empty = trades:dict_get(TID, Dict0),
    Dict = trades:dict_write(trades:new(NewHeight, TID), Dict0),

    true = Nonce >=
        case CID1 of
            <<0:256>> ->
                A1 = accounts:dict_get(Acc1, Dict),
                A1#acc.nonce;
            %A1N = A1#acc.nonce,
            %    true = A1N < Nonce,
            %    A1_2 = A1#acc{
            %             nonce = A1N + 1
            %            },
            %     accounts:dict_write(A1_2, Dict);
        _ ->
                Key1 = sub_accounts:make_key(Acc1, CID1, Type1),
                A1 = sub_accounts:dict_get(Key1, Dict),
                A1#sub_acc.nonce
%                A1N = A1#sub_acc.nonce,
%                true = A1N < Nonce,
%                A1_2 = A1#sub_acc{
%                         nonce = A1N + 1
%                        },
%                sub_accounts:dict_write(A1_2, Dict)
        end,

    Dict2 = fee_helper(Fee1, Acc1, Dict),
    Dict3 = fee_helper(Fee - Fee1, Acc2, Dict2),
    Dict4 = move_helper(Acc1, Acc2, Amount1, CID1, Type1, Dict3),
    Dict5 = move_helper(Acc2, Acc1, Amount2, CID2, Type2, Dict4),
    Dict5.

fee_helper(Fee, Add, Dict) ->
    if
        Fee == 0 -> Dict;
        true ->
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
    
trade_id_maker(Acc1, Salt) ->
    <<_:256>> = Salt,
    hash:doit(
      <<Acc1/binary, 
        Salt/binary>>).
    

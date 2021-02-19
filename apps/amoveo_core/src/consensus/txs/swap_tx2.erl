-module(swap_tx2).
-export([go/4, make_offer/11, make_dict/4]).
-include("../../records.hrl").
          

make_dict(From, SNCOffer, MatchParts, Fee) ->
    Acc = trees:get(accounts, From),
    Nonce = Acc#acc.nonce + 1,
    #swap_tx2{from = From, offer = SNCOffer, 
              nonce = Nonce,
              fee = Fee, match_parts = MatchParts}.
make_offer(From, StartLimit, EndLimit, 
           CID1, Type1, Amount1, 
           CID2, Type2, Amount2, 
           Parts, Fee1) ->
    Salt = crypto:strong_rand_bytes(32),
    TID = swap_tx:trade_id_maker(From, Salt),
    Trade = trees:get(trades, TID),
    Nonce = case Trade of
                empty -> 1;
                #trade{height = H} -> H
            end,
    #swap_offer2{
                 acc1 = From,
                 start_limit = StartLimit,
                 end_limit = EndLimit,
                 cid1 = CID1,
                 type1 = Type1,
                 amount1 = Amount1, 
                 cid2 = CID2,
                 type2 = Type2,
                 amount2 = Amount2,
                 salt = Salt,
                 start_nonce = Nonce,
                 parts = Parts %this is how many sub-parts the limit order can be broken up into and matched. It is also how much the trade nonce can increase by until the limit order is no longer valid.
           }.
go(Tx, Dict0, NewHeight, NonceCheck) ->
    #swap_tx2{
    from = Acc2,
    offer = SNCO,
    match_parts = MatchParts,
    nonce = Nonce0,
    fee = Fee
   } = Tx,
    true = NewHeight > forks:get(44),
    true = is_integer(MatchParts),
    true = MatchParts > 0,
    true = signing:verify(SNCO),
    NCO = signing:data(SNCO),
    #swap_offer2{acc1 = Acc1,
                 start_limit = SL,
                 end_limit = EL,
                 cid1 = CID1,
                 type1 = Type1,
                 amount1 = Amount1,
                 cid2 = CID2,
                 type2 = Type2,
                 amount2 = Amount2,
                 salt = Salt,
                 start_nonce = StartNonce,
                 parts = Parts
                } = NCO,
    true = NewHeight >= SL,
    true = NewHeight =< EL,
    TID = swap_tx:trade_id_maker(Acc1, Salt),
    Trade = trades:dict_get(TID, Dict0),
    CurrentNonce = 
        case Trade of
            empty -> 1;
            _ -> #trade{height = CN} = Trade,
                 CN
        end,
    NextNonce = CurrentNonce + MatchParts,
    %NextNonce = 
    %    case Trade of
    %        empty -> 1 + MatchParts;
    %        _ -> 
    %            #trade{height = CurrentNonce} = Trade,
    %            CurrentNonce +
    %                MatchParts
    %    end,
    true = NextNonce > StartNonce,
    true = NextNonce =< (StartNonce + Parts),
    Trade2 = case Trade of
                 empty -> trades:new(NextNonce, TID);
                 _ -> Trade#trade{height = NextNonce}
             end,
    Dict2 = trades:dict_write(Trade2, Dict0),
    A1 = Amount1 * MatchParts div Parts,
    A2 = Amount2 * MatchParts div Parts,
    Nonce = if
                NonceCheck -> Nonce0;
                true -> none
            end,
    A2Fee = accounts:dict_update(
              Acc2, Dict2, -Fee, Nonce),
    Dict3 = accounts:dict_write(A2Fee, Dict2),
    Dict4 = swap_tx:move_helper(
              Acc1, Acc2, A1, 
              CID1, Type1, Dict3),
    Dict5 = swap_tx:move_helper(
              Acc2, Acc1, A2, 
              CID2, Type2, Dict4),
    %io:fwrite(packer:pack(trees:get(accounts, Acc2))),
    %io:fwrite("\n"),
    F45 = forks:get(45),
    if 
        (Parts == 1) and (NewHeight > F45) ->
            R = receipts:new(TID, Acc2),
            empty = receipts:dict_get(receipts:id(R), Dict5),
            receipts:dict_write(R, Dict5);
        true ->
            Dict5
    end.

    
                                  
        

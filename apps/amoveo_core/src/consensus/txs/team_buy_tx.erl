-module(team_buy_tx).
-export([go/4, make_dict/11, hash/1]).
-include("../../records.hrl").


make_dict(From, Fee, Pubkeys, Amounts, CH, MT, Source, SourceType, Matrix, StartLimit, EndLimit) ->
    %Acc = trees:get(accounts, From),
    %Nonce = Acc#acc.nonce,
    Salt = crypto:strong_rand_bytes(32),
    NewCID = contracts:make_id(CH, length(hd(Matrix)), Source, SourceType),
    #team_buy_tx{
                  from = From, %nonce = Nonce + 1,
                  fee = Fee, pubkeys = Pubkeys,
                  amounts = Amounts, sigs = [],
                  contract_hash = CH,
                  many_types = MT, source = Source,
                  source_type = SourceType,
                  matrix = Matrix, salt = Salt,
                  new_cid = NewCID,
                  start_limit = StartLimit, 
                  end_limit = EndLimit}.

go(Tx, Dict, NewHeight, _) ->
    #team_buy_tx{
    from = From,
    %nonce = Nonce,
    fee = Fee,
    pubkeys = Pubkeys,
    amounts = Amounts,
    sigs = Sigs,
    contract_hash = CH,
    new_cid = NewCID,
    many_types = MT,
    source = SourceCID,
    source_type = SourceType,
    matrix = Matrix,
    salt = Salt,
    start_limit = SL,
    end_limit = EL
   } = Tx,

    MCF = governance:dict_get_value(max_contract_flavors, Dict),
    RL = length(hd(Matrix)),
    true = length(Matrix) == length(Pubkeys),
    true = length(Sigs) == length(Pubkeys),
    true = length(Amounts) == length(Pubkeys),
    true = (RL =< MCF),

    true = SL =< NewHeight,
    true = EL >= NewHeight,

    H = hash(Tx),%this is what they all signed.

    empty = trades:dict_get(H, Dict),
    Dict1 = trades:dict_write(
              trades:new(NewHeight, H), 
              Dict),

    true = verify_sigs(H, Sigs, Pubkeys),

    <<TwoE32:32>> = <<-1:32>>,
    %TwoE32 (2 ^ 32 - 1), biggest number expressable in 4 bytes, the biggest number in chalang's integer notation.
    all_length(RL, Matrix),
    true = contract_evidence_tx:column_sum(TwoE32, Matrix),

    Facc = accounts:dict_update(From, Dict1, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict1),

    Dict3 = source_changes(Pubkeys, Amounts, SourceCID, SourceType, Dict2),

    Amount = sum_up(Amounts),%how much subcurrency we are creating.

    NewCID = contracts:make_id(CH, RL, SourceCID, SourceType),
    C1 = case contracts:dict_get(NewCID, Dict3) of
             empty ->
                 true = (Amount >= 0),
                 contracts:new(CH, RL, SourceCID, SourceType);
             X ->
                 true = ((X#contract.volume + Amount) >= 0),
                 X
         end,
    C2 = C1#contract{
           volume = Amount + C1#contract.volume
          },
    Dict4 = contracts:dict_write(C2, Dict3),
    give_subcurrencies(Amount, Pubkeys, NewCID, Matrix, Dict4).

sum_up([]) -> 0;
sum_up([H|T]) -> 
    H + sum_up(T).

hash(Tx) ->
    #team_buy_tx{
    %from = From,
    %fee = Fee,
    pubkeys = Pubkeys,
    amounts = Amounts,
    sigs = Sigs,
    contract_hash = CH,
    many_types = MT,
    source = SourceCID,
    source_type = SourceType,
    matrix = Matrix,
    salt = Salt,
    start_limit = SL,
    end_limit = EL
     } = Tx,
    <<_:256>> = Salt,
    hash:doit([Pubkeys, Amounts, CH, MT, SourceCID, SourceType, Matrix, SL, EL, Salt]).
    
verify_sigs(X, [], []) -> true;
verify_sigs(X, [Sig|S], [Pub|P]) -> 
    testnet_sign:verify_sig(X, Sig, Pub) and
        verify_sigs(X, S, P).
all_length(_, []) -> ok;
all_length(L, [H|T]) -> 
    L = length(H),
    all_length(L, T).

%everyone pays some source currency to participate in the contract.
source_changes([], [], _, _, Dict) ->
    Dict;
source_changes([Pub|P], [0|A], CID, Type, Dict) ->
    source_changes(P, A, CID, Type, Dict);
source_changes([Pub|P], [Amount|A], CID, Type, Dict) ->
    true = Amount > 0,
    Dict2 = 
        case CID of
            <<0:256>> ->
                Acc = accounts:dict_update(Pub, Dict, -Amount, none),
                accounts:dict_write(Acc, Dict);
            _ ->
                Key = sub_accounts:make_key(Pub, CID, Type),
                Acc = sub_accounts:dict_get(Key, Dict),
                NewBal = Acc#sub_acc.balance - 
                    Amount,
                true = (NewBal > -1),
                Acc2 = Acc#sub_acc{
                       balance = NewBal
                      },
                sub_accounts:dict_write(Acc2, Dict)
        end,
    source_changes(P, A, CID, Type, Dict2).
                    
                    
give_subcurrencies(
  _, [], _, [], Dict) -> 
    Dict;
give_subcurrencies(
  Amount, [Acc|A], 
  CID, [Row|M], Dict) -> 
    Dict2 = give_subcurrencies2(Amount, Acc, CID, Row, 1, Dict),
    give_subcurrencies(Amount, A, CID, M, Dict2).
give_subcurrencies2(
  _, _, _, [], _, Dict) -> 
    Dict;
give_subcurrencies2(
  Amount, Acc, CID, [0|Row], N, Dict) -> 
    give_subcurrencies2(
      Amount, Acc, CID, Row, N+1, Dict);
give_subcurrencies2(
  Amount, Acc, CID, 
  [<<A:32>>|Row], N, Dict) ->
    <<TwoE32:32>> = <<-1:32>>,
    A2 = Amount * A div TwoE32,
    Key = sub_accounts:make_key(Acc, CID, N),
    SA2 = 
        case sub_accounts:dict_get(Key, Dict) of
            empty ->
                sub_accounts:new(Acc, A2, CID, N);
            SA ->
                B1 = SA#sub_acc.balance,
                SA#sub_acc{
                  balance = B1 + A2
                 }
        end,
    Dict2 = sub_accounts:dict_write(SA2, Dict),
    give_subcurrencies2(
      Amount, Acc, CID,
      Row, N+1, Dict2).
    


-module(stablecoin_new_tx).
-export([go/4, make_dict/13, id_maker/2]).

%for creating perpetual stablecoins.
-include("../../records.hrl").


make_dict(From, Salt, Source, SourceType,
          CodeHash, 
          TDuration, UDuration, Period,
          Expiration,
          UTrigger, CStep, 
          Margin,
          Fee) ->
    #stablecoin_new_tx{
           from = From,
           id = Salt,
           source = Source,
           source_type = SourceType,
           code_hash = CodeHash,
           timelimit_auction_duration = TDuration,
           undercollateralization_auction_duration = UDuration,
           undercollateralization_price_trigger = UTrigger,
           collateralization_step = CStep,
           period = Period,
           margin = Margin,
           expiration = Expiration,
           fee = Fee
          }.

go(Tx, Dict, NewHeight, _) ->
    #stablecoin_new_tx{
    from = From,
    id = Salt,
    source = Source,
    source_type = SourceType,
    code_hash = CodeHash,
    timelimit_auction_duration = TDuration,
    undercollateralization_auction_duration = UDuration,
    undercollateralization_price_trigger = UTrigger,
    collateralization_step = CStep,
    period = Period,
    margin = Margin,
    expiration = Expiration,
    fee = Fee
   } = Tx,
    true = NewHeight > forks:get(46),

    %charge the fee.
    Dict2 = swap_tx:fee_helper(Fee, From, Dict),

    %create the finite stablecoin contract based on codeHash, the current height, and other data from this stablecoin.
    true = Expiration =< (Period + NewHeight),
    %true = Expiration > ((Period * 9 div 10) + NewHeight),
    true = Expiration > NewHeight,
    HB = constants:height_bits(),
    Code = <<2, 6, (<<Margin:48>>)/binary, 0, (<<Expiration:32>>)/binary, 2, 32, CodeHash/binary, 113>>,
    CH = hash:doit(Code),
    CID = contracts:make_id(CH, 2, Source, SourceType),
    empty = contracts:dict_get(CID, Dict2),
    NC = contracts:new(CH, 2, Source, SourceType),
    Dict3 = contracts:dict_write(NC, Dict2),
    

    %check that the stablecoin doesn't already exist.
    ID = id_maker(From, Salt),
    empty = stablecoins:dict_get(ID, Dict3),

    %create the stablecoin.
    PB = 8*constants:pubkey_size(),
    Stablecoin = #stablecoin{
      id = ID,
      auction_mode = false,
      source = Source,
      amount = 0,
      code_hash = CodeHash,
      timeout = NewHeight + TDuration,
      max_bid_pubkey = <<0:PB>>,
      max_bid_amount = 0,
      timelimit_auction_duration = TDuration,
      undercollateralization_auction_duration = UDuration,
      undercollateralization_price_trigger = UTrigger,
      collateralization_step = CStep,
      margin = Margin,
      period = Period
     },
    stablecoins:dict_write(Stablecoin, Dict3).
    

id_maker(Acc1, Salt) ->
    true = is_binary(Salt),
    true = (size(Salt) < 33),
    hash:doit(
      <<Acc1/binary, 
        Salt/binary>>).
    

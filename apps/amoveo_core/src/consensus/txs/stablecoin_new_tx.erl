-module(stablecoin_new_tx).
-export([go/4, make_dict/12]).

%for creating perpetual stablecoins.
-include("../../records.hrl").

-record(stablecoin_new_tx, {
     from,
     id,
     fee,
     source,
     source_type,
     amount,
     code_hash,
     timelimit_auction_duration,
     undercollateralization_auction_duration,
     undercollateralization_price_trigger,
     collateralization_step,
     period
}).

make_dict(From, ID, Source, SourceType,
          Amount, CodeHash, 
          TDuration, UDuration, Period,
          UTrigger, CStep, Fee) ->
    #stablecoin_new_tx{
           from = From,
           id = ID,
           source = Source,
           source_type = SourceType,
           amount = Amount,
           code_hash = CodeHash,
           timelimit_auction_duration = TDuration,
           undercollateralization_auction_duration = UDuration,
           undercollateralization_price_trigger = UTrigger,
           collateralization_step = CStep,
           period = Period,
           fee = Fee
          }.

go(Tx, Dict, NewHeight, _) ->
    #stablecoin_new_tx{
           from = From,
           id = ID,
           source = Source,
           source_type = SourceType,
           amount = Amount,
           code_hash = CodeHash,
           timelimit_auction_duration = TDuration,
           undercollateralization_auction_duration = UDuration,
           undercollateralization_price_trigger = UTrigger,
           collateralization_step = CStep,
           period = Period,
           fee = Fee
   } = Tx,

    %charge the fee.
    Dict2 = swap_tx:fee_helper(Fee, From, Dict),

    %create the finite stablecoin contract based on codeHash, the current height, and other data from this stablecoin.
    %TODO CH should be optimized.
    CH = <<>>,
    CID = contracts:make_id(CH, 2, Source, SourceType),
    empty = contracts:dict_get(CID, Dict2),
    NC = contracts:new(CH, 2, Source, SourceType),
    Dict3 = contracts:dict_write(NC, Dict2),
    

    %check that the stablecoin doesn't already exist.
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
      period = Period
     },

    stablecoins:dict_write(Stablecoin, Dict3).
    

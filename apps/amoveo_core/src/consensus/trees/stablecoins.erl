-module(stablecoins).
-export([
          %custom for this tree
	 write/2, get/2, delete/2,%update tree stuff
         %dict_update/9, 
         dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
         make_id/1,
	 all/0,
	 test/0]).

-include("../../records.hrl").

-record(stablecoin, {
          id,
          auction_mode, %can be: false, time_limit, under_coll
          source, %collateral contract id. for the finite stablecoin.
          source_amount, %amount of collateral locked in the perpetual stablecoin
          code_hash, %hash of code to decide if a collateral smart contract is valid
          timeout, %height at which an timelimit auction should start.
          max_bid_pubkey, %pubkey of whoever made the biggest bid so far.
          max_bid_amount, %how much they had bid.
          timelimit_auction_duration,
          undercollateralization_auction_duration,
          undercollateralization_price_trigger, %out of 100000, if this is 99000, and there are 100 veo worth of perpetual stablecoins, then you can use 99 veo to buy all the finite stablecoins backing the contract. This is like a lower bound on how collaterlized we want the contract to be.
          collateralization_step, %during each undercollateralization auction, how much should we attempt to increase the collateralization by. This should aim for about the midpoint of the range of collateralization we are targeting.
          period %how long until the next finite stablecoin expires. this should be longer than the timelimit_auction_duration

         }).
make_id(S) ->
    #stablecoin{
         id = ID
        } = S,
    ID.

serialize(S) ->
    #stablecoin{
           id = ID,
           auction_mode = Mode,
           source = Source,
           source_amount = Amount,
           code_hash = CodeHash,
           timeout = Timeout,
           max_bid_pubkey = MaxBidPubkey,
           max_bid_amount = MaxBidAmount,
           timelimit_auction_duration = TDuration,
           undercollateralization_auction_duration = UDuration,
           undercollateralization_price_trigger = UTrigger,
           collateralization_step = CStep,
           period = Period
          } = S,
    M = case Mode of
            false -> 0;
            time_limit -> 1;
            under_coll -> 2
        end, 
    <<_:256>> = ID,
    <<_:256>> = Source,
    <<_:256>> = CodeHash,
    true = Amount > -1,
    BB = constants:balance_bits(),
    HB = constants:height_bits(),
    true = Amount < math:pow(2, BB),
    true = Timeout > -1,
    true = Timeout < math:pow(2, HB),
    PB = 8*constants:pubkey_size(),
    <<_:PB>> = MaxBidPubkey, 
    true = MaxBidAmount > 0,
    true = MaxBidAmount < math:pow(2, BB),
    true = TDuration > 0,
    true = TDuration < math:pow(2, HB),
    true = UDuration > 0,
    true = UDuration < math:pow(2, HB),
    true = UTrigger > 0,
    true = UTrigger < 1000001,%20 bits
    true = CStep > 0,
    true = CStep < 1000001,%20 bits
    true = Period > TDuration,
    true = Period < math:pow(2, HB),
    <<
      ID/binary,
      Source/binary,
      CodeHash/binary,
      MaxBidPubkey/binary,
      M:8,
      Amount:BB,
      Timeout:HB,
      MaxBidAmount:BB,
      TDuration:HB,
      UDuration:HB,
      UTrigger:24,
      CStep:24,
      Period:HB>>.
deserialize(S) -> 
    BB = constants:balance_bits(),
    HB = constants:height_bits(),
    PB = 8*constants:pubkey_size(),
    HS = constants:hash_size()*8,
    <<ID:HS,
      Source:HS,
      CodeHash:HS,
      MaxBidPubkey:PB,
      M:8,
      Amount:BB,
      Timeout:HB,
      MaxBidAmount:BB,
      TDuration:HB,
      UDuration:HB,
      UTrigger:24,
      CStep:24,
      Period:HB>> = S,
    Mode = case M of
               0 -> false;
               1 -> time_limit;
               2 -> under_coll
           end,
    
    #stablecoin{
                 id = <<ID:HS>>,
                 auction_mode = Mode,
                 source = <<Source:HS>>,
                 source_amount = Amount,
                 code_hash = <<CodeHash:HS>>,
                 timeout = Timeout,
                 max_bid_pubkey = <<MaxBidPubkey:PB>>,
                 max_bid_amount = MaxBidAmount,
                 timelimit_auction_duration = TDuration,
                 undercollateralization_auction_duration = UDuration,
                 undercollateralization_price_trigger = UTrigger,
                 collateralization_step = CStep,
                 period = Period
               }.


write(Stablecoin, Root) ->
    ID = make_id(Stablecoin),
    M = serialize(Stablecoin),
    trie:put(key_to_int(ID), M, 0, Root, stablecoins). %returns a pointer to the new root

key_to_int(<<X:256>>) -> X.
get(ID, Tree) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Tree, stablecoins),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
delete(ID, Trees) ->
    trie:delete(ID, Trees, stablecoins).
all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Stablecoins = trees:stablecoins(Trees),
    All = trie:get_all(Stablecoins, stablecoins),
    lists:map(
      fun(Leaf) ->
	      deserialize(leaf:value(Leaf))
      end, All).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
dict_delete(Key, Dict) ->      
    dict:store({stablecoins, Key}, 0, Dict).
dict_get(Key, Dict) ->
    <<_:256>> = Key,
    X = dict:find({stablecoins, Key}, Dict),
    case X of
        error -> error;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> deserialize(Y)
    end.
dict_write(M, Dict) ->
   dict:store({stablecoins, M#stablecoin.id},
              serialize(M),
              Dict).
    
    
test() ->
    {NewPub,NewPriv} = signing:new_key(),
    A = #stablecoin{
      id = hash:doit(1),
      auction_mode = time_limit,
      source = hash:doit(2),
      source_amount = 100000000,
      code_hash = hash:doit(3),
      timeout = 1000,
      max_bid_pubkey = NewPub,
      max_bid_amount = 20000000,
      timelimit_auction_duration = 20,
      undercollateralization_auction_duration = 30,
      undercollateralization_price_trigger = 20000,
      collateralization_step = 10000,
      period = 50
     },
    %io:fwrite(size(serialize(A))),
    A = deserialize(serialize(A)),
    R = trees:empty_tree(stablecoins),
    NewLoc = write(A, R),
    ID = A#stablecoin.id,
    {Root, A, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(A), Proof),
    success.
    

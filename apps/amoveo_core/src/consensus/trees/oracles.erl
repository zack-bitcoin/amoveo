-module(oracles).
-export([new/9, set_orders/2, orders/1, %custom stuff
         write/2, get/2,%update tree stuff
         dict_get/2, dict_write/2, dict_write/3, %update dict stuff
	 meta_get/1, deserialize/1, all/0, 
	 ready_for_bets/0, ready_to_close/0,
	 verify_proof/4,make_leaf/3,key_to_int/1,serialize/1,test/0]). %common tree stuff
-define(name, oracles).
-include("../../records.hrl").
orders(X) -> X#oracle.orders.
set_orders(X, Orders) ->
    X#oracle{orders = Orders, orders_hash = orders:root_hash(Orders)}.
new(ID, Question, Starts, Creator, GovernanceVar, GovAmount, Dict, F10, Height) ->
    <<_:256>> = ID,
    true = size(Creator) == constants:pubkey_size(),
    %Height = api:height(),
    true = (GovernanceVar > -1) and (GovernanceVar < governance:max(Height)),
    Orders = if
		 F10 -> 0;
		 true -> orders:empty_book()%
	     end,
    MOT = governance:dict_get_value(minimum_oracle_time, Dict),
    #oracle{id = ID,
	    result = 0,
	    question = Question,
	    starts = Starts,
	    type = 3,%1 means we are storing orders of true, 2 is false, 3 is bad.
	    orders = Orders,
            orders_hash = <<0:256>>,
	    creator = Creator,
	    done_timer = Starts + MOT,
	    governance = GovernanceVar,
	    governance_amount = GovAmount
	   }.
all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Oracles = trees:oracles(Trees),
    All = trie:get_all(Oracles, oracles),
    lists:map(
      fun(Leaf) ->
	      X = oracles:deserialize(leaf:value(Leaf)),
	      QH = X#oracle.question,
	      Text = case oracle_questions:get(QH) of
			 error -> <<"unknown">>;
			 {ok, T} -> T
		     end,
	      {Text, X}
      end, All).
ready_to_close() ->
    A = all(),
    rtc2(A).
rtc2([]) -> [];
rtc2([{Text, Oracle}|T]) ->
    R = Oracle#oracle.result,
    D = Oracle#oracle.done_timer,
    H = block:height(),
    if
	(H < D) -> rtc2(T);
	(not (R == 0)) -> rtc2(T);
	true -> [{Text, Oracle}|rtc2(T)]
    end.
    
ready_for_bets() ->
    A = all(),
    rfb2(A).
rfb2([]) -> [];
rfb2([{Text, Oracle}|T]) ->
    R = Oracle#oracle.result,
    S = Oracle#oracle.starts,
    H = block:height(),
    if
	(H < S) -> rfb2(T);
	(not (R == 0)) -> rfb2(T);
	true -> [{Text, Oracle}|rfb2(T)]
    end.
    
		      
serialize(X) ->
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    Question = X#oracle.question,
    Orders = case X#oracle.orders of
                 0 -> X#oracle.orders_hash;
                 Z -> orders:root_hash(Z)
             end,
    HS = size(Question),
    HS = size(Orders),
    HB = constants:height_bits(),
    true = size(X#oracle.creator) == PS,
    true = size(Question) == HS,
    true = size(Orders) == HS,
    <<_:256>> = X#oracle.id,
    <<(X#oracle.id)/binary,
      (X#oracle.result):8,
      (X#oracle.type):8,
      (X#oracle.starts):HB,
      (X#oracle.done_timer):HB,
      (X#oracle.governance):8,
      (X#oracle.governance_amount):8,
      (X#oracle.creator)/binary,
      Question/binary,
      Orders/binary>>.
deserialize(X) ->
    PS = constants:pubkey_size()*8,
    HS = constants:hash_size()*8,
    HEI = constants:height_bits(),
    <<ID:HS,
     Result:8,
     Type:8,
     Starts:HEI,
     DT:HEI,
     Gov:8,
     GovAmount:8,
     Creator:PS,
     Question:HS,
     Orders:HS
     >> = X,
    #oracle{
           id = <<ID:256>>,
           type = Type,
           result = Result,
           starts = Starts,
           question = <<Question:HS>>,
           creator = <<Creator:PS>>,
           done_timer = DT,
           governance = Gov,
           governance_amount = GovAmount,
           orders_hash = <<Orders:HS>>
      }.
dict_write(Oracle, Dict) ->
    dict_write(Oracle, 0, Dict).
dict_write(Oracle, Meta, Dict) ->
    Key = Oracle#oracle.id,
    dict:store({oracles, Key},
               {serialize(Oracle), Meta},
               Dict).
meta_get(X) ->
    X#oracle.orders.
write(Oracle, Root) ->
    %meta is a pointer to the orders tree.
    V = serialize(Oracle),
    Key = Oracle#oracle.id,
    Meta = Oracle#oracle.orders,
    trie:put(key_to_int(Key), V, Meta, Root, ?name).
dict_get(ID, Dict) ->
    <<_:256>> = ID,
    X = dict:find({oracles, ID}, Dict),
    case X of
	error -> empty;
        {ok, 0} -> empty;
        {ok, {0, _}} -> empty;
        {ok, {Y, Meta}} ->
            Y2 = deserialize(Y),
            Y2#oracle{orders = Meta}
    end.
key_to_int(X) -> 
    %<<Y:256>> = hash:doit(<<X:256>>),
    <<_:256>> = X,
    <<Y:256>> = hash:doit(X),
    Y.
get(ID, Root) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Root, ?name),
    V = case Leaf of 
	    empty -> empty;
	    L -> 
		X = deserialize(leaf:value(L)),
		M = leaf:meta(L),
		X#oracle{orders = M}
	end,
    {RH, V, Proof}.
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), 
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
    

test() ->
    %headers:dump(),
    %block:initialize_chain(),
    %tx_pool:dump(),
    %Trees = (tx_pool:get())#tx_pool.trees, %depreciated
    %Root0 = constants:root0(),
    %X0 = new(1, hash:doit(1), 2, constants:master_pub(), constants:initial_difficulty(), 0, 0, Trees),
    %X = set_result(X0, 3),
    %X2 = deserialize(serialize(X)),
    %X3 = X2#oracle{orders = X#oracle.orders},
    %X = X3,
    %NewLoc = write(X, Root0),
    %{Root1, X, Path1} = get(X#oracle.id, NewLoc),
    %{Root2, empty, Path2} = get(X#oracle.id, Root0),
    %true = verify_proof(Root1, X#oracle.id, serialize(X), Path1),
    %true = verify_proof(Root2, X#oracle.id, 0, Path2),
    test2().
test2() ->
    %Trees = (tx_pool:get())#tx_pool.trees, %depreciated
    %OID = 2,
    %Root0 = constants:root0(),
    %X0 = new(OID, hash:doit(1), 2, constants:master_pub(), constants:initial_difficulty(), 0, 0, Trees),
    %io:fwrite("test oracle "),
    %io:fwrite(packer:pack(X0)),
    %io:fwrite("\n"),
    %Dict0 = dict:new(),
    %Dict1 = dict_write(X0, orders(X0), Dict0),
    %io:fwrite(packer:pack(dict:fetch_keys(Dict1))),
    %io:fwrite("\n"),
    %X0 = dict_get(OID, Dict1),
    success.
    

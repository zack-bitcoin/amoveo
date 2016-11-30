-module(to_channel_tx).%used to create a channel, or increase the amount of money in it.
-export([next_top/2,doit/7,tc_increases/1,to_channel/4,create_channel/5,create_channel2/6,my_side/1,min_ratio/2,test/0,grow_ratio/2,is_tc/1,id/1]).
-record(tc, {acc1 = 0, acc2 = 1, nonce = 0, bal1 = 0, bal2 = 0, consensus_flag = <<"delegated_1">>, fee = 0, id = -1, increment = 0}).
is_tc(X) -> is_record(X, tc).
id(X) -> X#tc.id.
    
my_side(Tx) ->
    A1 = Tx#tc.acc1,
    B1 = Tx#tc.bal1,
    A2 = Tx#tc.acc2,
    B2 = Tx#tc.bal2,
    case keys:id() of
	A1 -> B1;
	A2 -> B2
    end.
%half_them(Tx) ->
%    A1 = Tx#tc.acc1,
%    A2 = Tx#tc.acc2,
%    B = Tx#tc.bal1 < Tx#tc.bal2,
%    case keys:id() of
%	A1 -> B;
%	A2 -> (not B)
%    end.
good_key(S) -> ((S == <<"delegated_1">>) or (S == <<"delegated_2">>)).
create_channel(To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
%When creating a new channel, you don't choose your own ID for the new channel. It will be selected for you by next available.    
    Id = keys:id(),
    create_channel2(Id, To, MyBalance, TheirBalance, ConsensusFlag, Fee).
create_channel2(Id, To, MyBalance, TheirBalance, ConsensusFlag, Fee) ->
    Acc = block_tree:account(Id),
    ToAcc = block_tree:account(To),
    true = accounts:balance(Acc) > MyBalance,
    true = accounts:balance(ToAcc) > TheirBalance,
    S = ConsensusFlag,
    true = good_key(S),
    #tc{acc1 = Id, acc2 = To, nonce = accounts:nonce(Acc) + 1, bal1 = MyBalance, bal2 = TheirBalance, consensus_flag = ConsensusFlag, fee = Fee, increment = MyBalance + TheirBalance}.
    
to_channel(ChannelId, Inc1, Inc2, Fee) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    Channel = block_tree:channel(ChannelId),
    S = channels:type(Channel),
    true = good_key(S),
    #tc{acc1 = channels:acc1(Channel), acc2 = channels:acc2(Channel), bal1 = channels:bal1(Channel) + Inc1, bal2 = channels:bal2(Channel) + Inc2, consensus_flag = S, id = ChannelId, fee = Fee, nonce = accounts:nonce(Acc) + 1, increment = Inc1 + Inc2}.
%testnet_sign:set_revealed(SignedTx, ChannelId).

next_top(DBroot, Channels) -> next_top_helper(channels:array(), channels:top(), DBroot, Channels).
next_top_helper(Array, Top, DBroot, Channels) ->
    EmptyAcc = channels:empty(),
    case block_tree:channel(Top, DBroot, Channels) of
	EmptyAcc -> Top;
	_ ->
	    <<A:Top,_:1,B/bitstring>> = Array,
	    NewArray = <<A:Top,1:1,B/bitstring>>,
	    NewTop = channels:walk(Top, NewArray),
	    next_top_helper(NewArray, NewTop, DBroot, Channels)
    end.
doit(SignedTx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    Tx = testnet_sign:data(SignedTx),
    NewId = if
	Tx#tc.id == -1 -> testnet_sign:revealed(SignedTx);
	true -> Tx#tc.id
    end,
    From = Tx#tc.acc1,
    false = From == Tx#tc.acc2,
    Channel = block_tree:channel(NewId, ParentKey, Channels),
    Acc1 = block_tree:account(Tx#tc.acc1, ParentKey, Accounts),
    Acc2 = block_tree:account(Tx#tc.acc2, ParentKey, Accounts),
    EmptyChannel = channels:empty(),
    true = Tx#tc.bal1 > -1,
    true = Tx#tc.bal2 > -1,
    true = is_integer(Tx#tc.bal1),
    true = is_integer(Tx#tc.bal2),
    {Increment, Balance1, Balance2, Type, NewId2} = 
	if
	    Channel == EmptyChannel ->
		NI2 = next_top(ParentKey, Channels),
		B1 =  - Tx#tc.bal1 - Tx#tc.fee,
		B2 =  - Tx#tc.bal2 - Tx#tc.fee,
		I = Tx#tc.bal1 + Tx#tc.bal2,
		I = Tx#tc.increment,
		true = I > (TotalCoins div constants:max_channel()),%The channel has a minimum possible balance.
		T = Tx#tc.consensus_flag,
		true = good_key(T),
						%check if one of the pubkeys is keys:pubkey().
						%If so, then add it to the mychannels module.
		{I, B1, B2, T, NI2};
	    true ->
		T = channels:type(Channel),
		NI2 = Tx#tc.id,
		AccN1 = channels:acc1(Channel),
		AccN1 = Tx#tc.acc1,
		AccN2 = channels:acc2(Channel),
		AccN2 = Tx#tc.acc2,
		OldVol = channels:bal1(Channel) + channels:bal2(Channel),
		NewVol = Tx#tc.bal1 + Tx#tc.bal2,
		I = NewVol - OldVol,
		I = Tx#tc.increment,
		true = (-1 < I),%to_channel can only be used to increase the amount of money in a channel, for consensus reasons. 
		B1 = - Tx#tc.bal1 + channels:bal1(Channel) - Tx#tc.fee,
		B2 = - Tx#tc.bal2 + channels:bal2(Channel) - Tx#tc.fee,
		{I, B1, B2, T, NI2}
	end,
    Nonce = accounts:nonce(Acc1),
    Nonce = Tx#tc.nonce - 1,
    {D1, D2} =
	case Tx#tc.consensus_flag of
	    <<"delegated_1">> -> 
	    {Increment, 0};
	<<"delegated_2">> -> 
	    {0, Increment}
    end,
    N1 = accounts:update(Acc1, NewHeight, Balance1, D1, 1, TotalCoins),
    N2 = accounts:update(Acc2, NewHeight, Balance2, D2, 0, TotalCoins),
    true = NewId2 < constants:max_channel(),
    MyKey = keys:id(),
    %APub1 = accounts:pub(Acc1),
    %APub2 = accounts:pub(Acc2),
    Ch = channels:new(Tx#tc.acc1, Tx#tc.acc2, Tx#tc.bal1, Tx#tc.bal2, Type),
    CM_current = channel_manager:read(NewId2),
    if
	%channel == emptychannel means that we are creating a new channel, rather than increasing the amount of money in an existing one.
	((CM_current == empty) and ((Channel == EmptyChannel) and ((Tx#tc.acc1 == MyKey) or (Tx#tc.acc2 == MyKey)))) -> 
	    channel_partner:new_channel(NewId2, Ch, Accounts),
	    channel_manager_feeder:new_channel(NewId2, Ch, Accounts);
	true -> 1=1
    end,
    NewAccounts1 = dict:store(Tx#tc.acc1, N1, Accounts),
    NewAccounts = dict:store(Tx#tc.acc2, N2, NewAccounts1),
    NewChannels = dict:store(NewId2, Ch, Channels),
    {NewChannels, NewAccounts, TotalCoins, S}.
tc_increases(Txs) -> tc_increases(Txs, 0).
tc_increases([], X) -> X;
tc_increases([SignedTx|T], X) -> 
    Tx = testnet_sign:data(SignedTx),
    case element(1, Tx) of
	tc -> tc_increases(T, X+Tx#tc.increment);
	_ -> tc_increases(T, X)
    end.

min_ratio(Max, Tx) ->%This works for new channels. We need a seperate one for adding funds to an existing channel.
    B1 = Tx#tc.bal1,
    B2 = Tx#tc.bal2,
    B = B2 + B1,
    ratio_helper(B1, B2, B, Max, Tx).
ratio_helper(B1, B2, B, Max, Tx) ->
    A1 = Tx#tc.acc1,
    A2 = Tx#tc.acc2,
    F = case keys:id() of
	A1 -> fractions:new(B1, B);
	A2 -> fractions:new(B2, B)
    end,
    (not fractions:less_than(Max, F)).
grow_ratio(Max, Tx) ->
    Channel = block_tree:channel(Tx#tc.id, tx_pool:channels()),
    I1 = Tx#tc.bal1 - channels:bal1(Channel),
    I2 = Tx#tc.bal2 - channels:bal2(Channel),
    I = I1 + I2,
    ratio_helper(I1, I2, I, Max, Tx).

test() ->
    TxThem = #tc{bal1 = 0, bal2 = 10},
    TxMe = #tc{bal1 = 10, bal2 = 0},
    TxThema = #tc{bal1 = 1, bal2 = 10},
    TxMea = #tc{bal1 = 10, bal2 = 1},
    TxHalf = #tc{bal1 = 10, bal2 = 10},
    Tx = #tc{acc1 = 1, acc2 = 0,nonce = 1,bal1 = 1120000,bal2 = 1100000,fee = 50,id = -1, increment = 2220000},
    true = min_ratio(fractions:new(2, 3), Tx),
    true = min_ratio(fractions:new(1, 2), TxThem),
    false = min_ratio(fractions:new(1, 2), TxMe),
    true = min_ratio(fractions:new(1, 2), TxThema),
    false = min_ratio(fractions:new(1, 2), TxMea),
    true = min_ratio(fractions:new(1, 2), TxHalf),
    success.
  

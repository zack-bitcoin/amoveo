-module(block).
-export([block_to_header/1, %get_by_height_in_chain/2,
         get_by_height/1, hash/1, get_by_hash/1, 
         initialize_chain/0, make/4,
         mine/1, mine/2, mine2/2, check/1, check0/1, check2/2, check3/2, root_hash_check/4,
         top/0, genesis_maker/0, height/0, bottom/0,verify/1,

	 time_now/0, all_mined_by/1, time_mining/1,
	 period_estimate/0, hashrate_estimate/0,
	 period_estimate/1, hashrate_estimate/1,
	 hashes_per_block/0, hashes_per_block/1,
         no_counterfeit/4,
         miner_fees/1, gov_fees/3,
         header_by_height/1, 
         calculate_prev_hashes/1,
         prev_hash/2,
         make_roots/1, sum_amounts_helper/5,
         test/0]).
%Read about why there are so many proofs in each block in docs/design/light_nodes.md
-include("../../records.hrl").
-record(roots, {accounts, channels, existence, oracles, governance}).%
-record(roots2, {accounts, channels, existence, oracles, governance, matched, unmatched}).%
-record(roots3, {accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades}).%
-record(roots4, {accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets}).%
-record(roots5, {accounts, channels, existence, oracles, governance, matched, unmatched, sub_accounts, contracts, trades, markets, receipts, stablecoins}).

tx_hash(T) -> hash:doit(T).
proof_hash(P) -> hash:doit(P).
merkelize_thing(X) when is_binary(X) -> X;
merkelize_thing(X) when is_tuple(X) and (size(X) > 0)->
    T = element(1, X),
    case T of
        proof -> proof_hash(X);
        _ -> tx_hash(X)
    end;
merkelize_thing(X) -> hash:doit(X).
    
merkelize_pair(A, B) ->
    C = [merkelize_thing(A), merkelize_thing(B)],
    hash:doit(C).
merkelize([A]) -> merkelize_thing(A);
merkelize([A|[B|T]]) ->
    merkelize(merkelize2([A|[B|T]]));
merkelize([]) -> <<0:256>>.
merkelize2([]) -> [];
merkelize2([A]) -> [merkelize_thing(A)];
merkelize2([A|[B|T]]) ->
    [merkelize_pair(A, B)|
     merkelize2(T)].

    
txs_proofs_hash(Txs, Proofs) ->
    TB = merkelize(Txs),
    PB = merkelize(Proofs),
    X = <<TB/binary, PB/binary>>,
    hash:doit(X).
block_to_header(B) ->
    if
        (B == error) -> 1=2;
        true -> ok
    end,
%    io:fwrite("block to header "),
%    io:fwrite(integer_to_list(B#block.height)),
%    io:fwrite("\n"),
    BV = [{1, B#block.market_cap},
	  {2, B#block.channels_veo},
	  {3, B#block.live_channels},
	  {4, B#block.many_accounts},
	  {5, B#block.many_oracles},
	  {6, B#block.live_oracles}],
    %todo, it is important that the proofs part is cleanly merkelized, so that we can replace it with it's hash in a compressed version of history.
    %proofs is currently appending a tuple to the list, so it breaks.
    StateRoot = 
        if
            %is_binary(B#block.proofs) ->
            is_tuple(B#block.proofs) -> 
                merkelize([{0, B#block.proofs}] ++ 
                              BV ++ B#block.txs);
            true ->
                merkelize(BV ++ B#block.txs ++ 
                              B#block.proofs)
        end,
    headers:make_header(
      B#block.prev_hash,
      B#block.height,
      B#block.time,
      B#block.version,
      B#block.trees_hash,
      StateRoot,
      B#block.nonce,
      B#block.difficulty,
      B#block.period).

hash(error) -> 1=2;
hash(B) when is_binary(B) ->%accepts binary headers
    case size(B) == constants:hash_size() of
        true ->
            B;
        false ->
            hash:doit(B)
    end;
hash(B) when element(1, B) == header ->
    hash(headers:serialize(B));
hash(B) when is_record(B, block) ->
    hash(block_to_header(B)).

calculate_prev_hashes(Parent) ->
    H = Parent#header.height,
    PH = hash(Parent),
    calculate_prev_hashes([PH], H, 2).

calculate_prev_hashes([PH|Hashes], Height, N) ->
    NHeight = Height - N,
    {ok, DBV} = application:get_env(amoveo_core, db_version),
    RH = if
             DBV > 1 -> block_db:ram_height();
             true -> 1
         end,
    case NHeight < RH of
%    case NHeight < 1 of
        true ->
            list_to_tuple([prev_hashes|lists:reverse([PH|Hashes])]);
        false ->
            %B0 = get_by_height_in_chain(NHeight+1, PH),
            %B = B0#block.prev_hash,
            %calculate_prev_hashes([B|[PH|Hashes]], NHeight, N*2)
            B = get_by_height_in_chain(NHeight, PH),
            calculate_prev_hashes([hash(B)|[PH|Hashes]], NHeight, N*2)
    end.
get_by_hash(H) -> 
    Hash = hash(H),
    %case block_db:read(Hash) of
    %io:fwrite("block:get by hash\n"),
    case block_db3:read(Hash) of
        error -> empty;
        <<>> -> empty;
        X -> X
    end.
            
%    BlockFile = amoveo_utils:binary_to_file_path(blocks, Hash),
%    case db:read(BlockFile) of
%        [] -> empty;
%        Block -> binary_to_term(zlib:uncompress(Block))
%    end.
bottom() ->
    %the lowest block (besides 0) that we have synced.
    %O((log(# blocks))^2)
    %binary search over our chain of headers, looking up a block each time until we find the empty slot.
    T = top(),
    case get_by_height(1) of
        error -> bottom(1, T#block.height);
        _ -> 0
    end.
bottom(M, N) when (N == M+1) -> N;
bottom(M, N) when (N > M) -> 
    %M is a height we don't have a block for.
    %N is a height we do have a block for.
    L = (M + N) div 2,
    case get_by_height(L) of
        error -> bottom(L, N);
        _ -> bottom(M, L)
    end.
            

top() -> 
    %O(log(# blocks))
    H = headers:top_with_block(),
    get_by_hash(hash(H)).
%    io:fwrite("block top start\n"),
%    io:fwrite(H),
%    top(H).%what we actually want is the highest header for which we have a stored block.
top(Header) ->
    false = element(2, Header) == undefined,
    case get_by_hash(hash(Header)) of
        empty -> 
            {ok, PrevHeader} = 
                headers:read(Header#header.prev_hash),
            top(PrevHeader);
        Block -> Block
    end.
%height() -> (top())#block.height.
height() ->
    TH = headers:top_with_block(),
    TH#header.height.
lg(X) when (is_integer(X) and (X > 0)) ->
    lgh(X, 0).
lgh(1, X) -> X;
lgh(N, X) -> lgh(N div 2, X+1).
header_by_height(N) ->
    header_by_height_in_chain(N, block:hash(headers:top())).
header_by_height_in_chain(N, Hash) when N > -1 ->
    {ok, H} = headers:read(Hash),
    M = H#header.height,
    D = M - N,
    if
        D == 0 -> H;
        true ->
            header_by_height_in_chain(N, H#header.prev_hash)
    end.
    
%get_by_height(275600) ->
%    get_by_hash(<<11,104,157,185,180,112,175,116,238,1,203,165,194,162,
%  170,112,91,209,249,9,142,146,59,137,192,225,144,165,110,
%  199,5,185>>);
get_by_height(N) ->
    %io:fwrite("block:get by height " ++ integer_to_list(N) ++ "\n"),
    L = block_db3:read(N, N),
    case L of
        [] -> error;
        error -> error;
        [X] -> X
    end.
             
get_by_height_old(N) ->
    {ok, DBV} = application:get_env(amoveo_core, db_version),
    if
        %((N == 0) and (DBV > 1)) -> block_db:genesis();
        ((N == 0) and (DBV > 1)) -> hd(block_db3:read(0, 0));
        true ->
            get_by_height_in_chain(N, headers:top_with_block())
    end.
    %get_by_height_in_chain(N, headers:top_with_block()).
get_by_height_in_chain(N, BH) when N > -1 ->
    %no longer being used.
    %if we are using the new database, and the block is more than fork_tolerance in history, then we should use the new way of looking up blocks by height.
    HN = block:height(),
    %{ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    RH = block_db:ram_height(),
    {ok, DBV} = application:get_env(amoveo_core, db_version),
    io:fwrite("block:get by height in chain\n"),
    if 
        %((N == 0) and (DBV > 1)) -> block_db:genesis();
        ((N == 0) and (DBV > 1)) -> hd(block_db3:read(0, 0));
        ((DBV > 1) and (N < RH)) -> 
            hd(block_db3:read(N, N));
%            block_db:by_height_from_compressed(
%              block_db:read_by_height(N), N);
        true ->
            Block = get_by_hash(hash(BH)),
    %io:fwrite(packer:pack(Block)),
            case Block of
            %case empty of
                empty ->
                    PrevHash = BH#header.prev_hash,
                    {ok, PrevHeader} = headers:read(PrevHash),
                    get_by_height_in_chain(N, PrevHeader);
                _  ->
                    M = Block#block.height,
                    D = M - N,
                    if
                        D < 0 -> empty;
                        D == 0 -> Block;
                        true ->
                            PrevHash = prev_hash(lg(D), Block),%TODO, instead we should walk backwards through the headers datastructure in linear time.
                            {ok, PrevHeader} = headers:read(PrevHash),
                            get_by_height_in_chain(N, PrevHeader)
                    end
            end
    end.
prev_hash(0, BP) -> BP#block.prev_hash;
prev_hash(N, BP) -> element(N+1, BP#block.prev_hashes).
time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis_maker() ->
    %Root0 = constants:root0(),
    Pub = constants:master_pub(),
    First = accounts:new(Pub, constants:initial_coins()),
    GovInit = governance:genesis_state(),
    Accounts = accounts:write(First, trees:empty_tree(accounts)),%This is leaking a small amount of memory, but it is probably too small to care about, since this function gets executed so rarely.
    Trees = trees:new(Accounts, 
                      trees:empty_tree(channels), 
                      trees:empty_tree(existence), 
                      ok, 
                      trees:empty_tree(oracles), 
                      GovInit),
    TreesRoot = trees:root_hash(Trees),
    BlockPeriod = governance:get_value(block_period, GovInit),
    HistoryString = <<"bitcoin 511599  0000000000000000005cdf7dafbfa2100611f14908ad99098c2a91719da93a50">>,
    Hash = hash:doit(HistoryString),
    #block{height = 0,
           prev_hash = Hash,%<<0:(constants:hash_size()*8)>>,
           txs = [],
           trees_hash = TreesRoot,
           time = 0,
           difficulty = constants:initial_difficulty(),
           period = BlockPeriod,
           version = version:doit(0),
           trees = Trees,
           roots = make_roots(Trees)
          }.
miner_fees([]) -> 0;
miner_fees([H|T]) ->
    element(4, signing:data(H)) + miner_fees(T).
   
tx_costs_dict([], _, Out) -> Out;
tx_costs_dict([STx|T], Dict, Out) ->
    Tx = signing:data(STx),
    Type = element(1, Tx),
    Cost = dict:fetch({governance, governance:name2number(Type)},
                      Dict),
    tx_costs_dict(T, Dict, Cost+Out).
tx_costs([], _, Out) -> Out;
tx_costs([STx|T], Governance, Out) ->
    Tx = signing:data(STx),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    tx_costs(T, Governance, Cost+Out).
market_cap(OldBlock, BlockReward, Txs0, Dict, Height) ->
    FH = forks:get(3),%
    if
	FH > Height ->
	    OldBlock#block.market_cap + 
		BlockReward - 
		gov_fees(Txs0, Dict, Height);
	Height == FH -> %
	    MC1 = OldBlock#block.market_cap + %
		BlockReward - %
		gov_fees(Txs0, Dict, Height),%
	    (MC1 * 6) div 5;%
	FH < Height ->%
	    DeveloperRewardVar = %
		governance:dict_get_value(developer_reward, Dict, Height),%
	    DeveloperReward = %
		(BlockReward * %
		 DeveloperRewardVar) div %
		10000,%
	    OldBlock#block.market_cap + %
		BlockReward - %
		gov_fees(Txs0, Dict, Height) + %
		DeveloperReward%
    end.
  
channel_rewards([], Accs) -> Accs;
channel_rewards([{Pub, Bal}|T], Accs) -> 
    {_, A, Proof} = accounts:get(Pub, Accs),
    if
        is_record(A, acc) ->
            A2 = A#acc{balance = 
                           A#acc.balance + Bal},
            Accs2 = accounts:write(A2, Accs),
            channel_rewards(T, Accs2);
        true ->
            channel_rewards(T, Accs)
    end.
   
governance_packer(L, DB) -> 
    case L of
        [] -> DB;
        [H|T] ->
            governance_packer(
              T, governance:write(H, DB))
    end.

    
 
trees_hash_maker(HeightCheck, Trees, NewDict4, 
                 ProofTree, RootHash) ->
    F10 = forks:get(10),
    F32 = forks:get(32),
    F35 = forks:get(35),
    F44 = forks:get(44),
    F48 = forks:get(48),
    F52 = forks:get(52),
    case HeightCheck of
        F10 -> true;
        F32 -> true;
        F35 -> true;
        F44 -> true;
        F48 -> true;
        F52 -> true;
        _ ->
            tree_data:dict_update_root(
              Trees, NewDict4, HeightCheck, 
              ProofTree, RootHash)
    end.


trees_maker(HeightCheck, Trees, NewDict4, ProofTree, RootHash) ->
    %io:fwrite("trees maker "),
    %io:fwrite(integer_to_list(HeightCheck)),
    %DEls = dict:fetch_keys(NewDict4),
    %io:fwrite(" "),
    %io:fwrite(integer_to_list(length(DEls))),
    %io:fwrite("\n"),
    if
        true -> ok;
        true -> 
    ND4_Keys = dict:fetch_keys(NewDict4),
            io:fwrite({lists:map(fun(X) -> {X, dict:fetch(X, NewDict4)} end, ND4_Keys)});
        true -> ok
    end,
    %io:fwrite({lists:map(fun(X) -> dict:find(X, NewDict4) end, ND4_Keys)}),
    %just governance 28, and a pubkey.
    NewTrees0 = 
        tree_data:dict_update_trie(
          Trees, 
          NewDict4, %maps 32 byte hash to #consensus_state objects
          HeightCheck, ProofTree, RootHash),
    F10 = forks:get(10),
    F32 = forks:get(32),
    F35 = forks:get(35),
    F44 = forks:get(44),
    F48 = forks:get(48),
    F52 = forks:get(52),
    GN = fun(Name, A) ->
                 V = governance:new(
                   governance:name2number(Name),
                       A)
         end,
    GD = fun(Name) ->
                 GN(Name, constants:encoded_fee())
         end,
    Trees2 = 
        if
            (HeightCheck == F10)  ->
                trees:trees1to2(NewTrees0);
            true ->
                NewTrees0
        end,
    Trees3 =
        if
            (HeightCheck == F32) ->
                GTF3 = governance_packer(
                        [GD(contract_new_tx),
                         GD(contract_use_tx),
                         GD(sub_spend_tx),
                         GD(contract_evidence_tx),
                         GD(contract_timeout_tx),
                         GD(contract_winnings_tx),
                         GD(contract_simplify_tx),
                         GN(max_contract_flavors, 32),
                         GD(swap_tx)],
                        trees:governance(Trees2)),
                T3 = trees:trees2to3(Trees2),
                T3#trees3{
                  governance = GTF3
                 };
            true -> Trees2
        end,
    Trees4 = 
        if
            (HeightCheck == F35) ->
                GTF4 = governance_packer(
                        [GD(market_new_tx),
                         GD(market_liquidity_tx),
                         GD(market_swap_tx),
                         GN(market_trading_fee,
                            constants:initial_trading_fee())%about 200000. it is out of 1 veo, so this is 0.2% 
                        ],
                        trees:governance(Trees3)),
                T4 = trees:trees3to4(Trees3),
                T4#trees4{
                  governance = GTF4
                 };
            true -> Trees3
        end,
    Trees5 =
        if
            (HeightCheck == F44) ->
                GTF5 = governance_packer(
                        [GD(swap_tx2),
                         GD(trade_cancel_tx)],
                        trees:governance(Trees4)),
                Trees4#trees4{
                  governance = GTF5
                 };
            true -> Trees4
        end,
    Trees6 = 
        if
            (HeightCheck == F48) ->
                GTF6 = governance_packer(
                        [GD(stablecoin_new_tx)
                        ],
                        trees:governance(Trees5)),
                T5 = trees:trees4to5(Trees5),
                T5#trees5{
                  governance = GTF6
                 };
            true -> Trees5
        end,
    Trees7 =
        if
            (HeightCheck == F52) ->
                io:fwrite("trees maker, about to merkle2verkle\n"),
                trees2:merkle2verkle(Trees6, 1);
            true -> Trees6
        end,
    ok,
    Trees7.
                

    
    
make(Header, Txs0, Trees, Pub) ->
    %{CB, _Proofs} = coinbase_tx:make(Pub, Trees),
    CB = coinbase_tx:make_dict(Pub),
    Txs = [CB|lists:reverse(Txs0)],
    Height = Header#header.height,
    Querys = proofs:txs_to_querys(Txs, Trees, Height+1),
%    if 
%        Height == 1 -> io:fwrite({Querys});
%        true -> ok 
%    end,
    Facts = proofs:prove(Querys, Trees, Height),
    Dict = proofs:facts_to_dict(Facts, dict:new()),
    if
        true -> ok;
        ((Height == 11) and (length(Txs0) > 0)) ->
            %io:fwrite({lists:map(fun(X) -> {X, dict:fetch(X, Dict)} end, dict:fetch_keys(Dict))});
            Fact = hd(tl(tl(tl(tl(tl(tl(tl(tl(tl(tl(tl(tl(Facts))))))))))))),
            Value = unmatched:deserialize(element(3, Fact)), %{unmatched, pub, <<0,...>>, 0, <<0,...>>}
            Value2 = unmatched:deserialize_head(element(3, Fact)),
            Pubkey = element(2, Value),
            Dict2 = proofs:facts_to_dict([Fact], dict:new()),
            Key = hd(dict:fetch_keys(Dict2)),
            <<OracleNum:256>> = element(3, Value),
            io:fwrite({OracleNum, Value2, Value, dict:fetch(Key, Dict2), base64:encode(Pubkey)});
        true -> ok;
        (Height == 10) and (1 == length(Txs0)) -> io:fwrite({Querys, facts, Facts});
        true -> ok
    end,
%    if
%        Height == 2 ->
%            io:fwrite({Facts, dict, dict:fetch(hd(dict:fetch_keys(Dict)), Dict)});
%        true -> ok
%    end,
    %Dict = proofs:facts_to_dict(Facts, dict:new()),%todo. this should work even if facts is a verkle proof.
    NewDict0 = txs:digest(Txs, Dict, Height + 1),
    if
        false ->
        %Height == 13 -> 
            %Keys0 = dict:fetch_keys(Dict),
            %io:fwrite({lists:map(fun(Y) -> dict:fetch(Y, Dict) end, Keys0)}),
            io:fwrite({Facts, 
                       dict, hd(dict:fetch_keys(Dict)), dict:fetch(hd(dict:fetch_keys(Dict)), Dict), 
                       new_dict0, hd(dict:fetch_keys(NewDict0)), element(1, dict:fetch(hd(dict:fetch_keys(NewDict0)), NewDict0))});
            %io:fwrite(NewDict0);
        true -> ok
    end,
    B = ((Height+1) == forks:get(5)),
    NewDict = if
		B -> %
		      OQL = governance:new(governance:name2number(oracle_question_liquidity), constants:oracle_question_liquidity()),%
		      %governance:dict_write(OQL, NewDict0);%
		      governance:dict_write_new(OQL, NewDict0);%
		true -> NewDict0
	    end,
    MinerAddress = Pub,
    FG6 = forks:get(6),
    FG9 = forks:get(9),
    MinerReward = miner_fees(Txs0),
    NewDict2 = if
		   (Height + 1) < FG6 -> NewDict;%
		   (Height + 1) < FG9 ->%
%    MinerAccount = accounts:dict_get(MinerAddress, Dict),%
		       MinerAccount = accounts:dict_update(MinerAddress, NewDict, MinerReward, none),%
		       accounts:dict_write(MinerAccount, NewDict);%
		   true ->
		       GovFees = gov_fees(Txs0, NewDict, Height),
		       %MinerAccount2 = accounts:dict_update(MinerAddress, NewDict, MinerReward - GovFees, none),
                       %HMA = trees2:hash_key(accounts, MinerAddress),
		       MinerAccount2 = accounts:dict_update(MinerAddress, NewDict, MinerReward - GovFees, none),
		       %MinerAccount2 = accounts:dict_update(HMA, NewDict, MinerReward - GovFees, none),
                       accounts:dict_write(MinerAccount2, NewDict)
	       end,
    NewDict4 = remove_repeats(NewDict2, Dict, Height + 1),
    %NewDict4 = NewDict2,%remove_repeats(NewDict2, NewDict0, Height + 1),

    HeightCheck = Height + 1,
    %io:fwrite("block make before tree maker\n"),
    NewTrees = trees_maker(HeightCheck, Trees, NewDict4, unknown, unknown),
    %io:fwrite("block make after tree maker\n"),

    %Governance = trees:governance(NewTrees),
    %Governance = trees:governance(Trees),
    %BlockPeriod = governance:get_value(block_period, Governance),
    BlockPeriod_gov = trees:get(governance, block_period, dict:new(), Trees),
    BlockPeriod = governance:value(BlockPeriod_gov),
    BlockReward_gov = trees:get(governance, block_reward, Dict, Trees),
    BlockReward = governance:value(BlockReward_gov),
    PrevHash = hash(Header),
    OldBlock = get_by_hash(PrevHash),
    %BlockReward = governance:get_value(block_reward, Governance),

    MarketCap = market_cap(OldBlock, BlockReward, Txs0, Dict, Height),
    TimeStamp = time_now(),
    NextHeader = #header{height = Height + 1, prev_hash = PrevHash, time = TimeStamp, period = BlockPeriod},
    Roots = make_roots(Trees),
    PrevStateHash = roots_hash(Roots),
    PrevStateHash = Header#header.trees_hash,
    NTreesHash = trees:root_hash(NewTrees),

    %NTreesHash = trees:root_hash2(NewTrees, Roots),
    %io:fwrite("block make finished\n"),
    Block = #block{height = Height + 1,
		   prev_hash = hash(Header),
		   txs = Txs,
		   trees_hash = NTreesHash,
		   time = TimeStamp,
		   difficulty = element(1, headers:difficulty_should_be(NextHeader, Header)),
                   period = BlockPeriod,

		   version = version:doit(Height+1),%constants:version(),
		   trees = NewTrees,
		   %prev_hashes = calculate_prev_hashes(Header),
		   prev_hashes = 0,
		   proofs = Facts,
                   roots = Roots,
		   %market_cap = OldBlock#block.market_cap + BlockReward - gov_fees(Txs0, Governance),
		   market_cap = MarketCap,
		   channels_veo = OldBlock#block.channels_veo + deltaCV(Txs0, Dict),
		   live_channels = OldBlock#block.live_channels + many_live_channels(Txs0),
		   many_accounts = OldBlock#block.many_accounts + many_new_accounts(Txs0),
		   many_oracles = OldBlock#block.many_oracles + many_new_oracles(Txs0),
		   live_oracles = OldBlock#block.live_oracles + many_live_oracles(Txs0)
		  },
    if
        true -> ok;
        (Height == 10) -> io:fwrite(Facts);
        true -> ok
    end,
    %io:fwrite("pack unpack check start 0\n"),
    Facts = packer:unpack(packer:pack(Facts)),
    %io:fwrite("pack unpack check start 1\n"),
    Block = packer:unpack(packer:pack(Block)),%maybe this is unnecessary?
    %io:fwrite("pack unpack check done \n"),
    %_Dict = proofs:facts_to_dict(Proofs, dict:new()),
    Block.
make_roots(Trees) when (element(1, Trees) == trees) ->%
    #roots{accounts = trie:root_hash(accounts, trees:accounts(Trees)),%
           channels = trie:root_hash(channels, trees:channels(Trees)),%
           existence = trie:root_hash(existence, trees:existence(Trees)),%
           oracles = trie:root_hash(oracles, trees:oracles(Trees)),%
           governance = trie:root_hash(governance, trees:governance(Trees))};%
make_roots(Trees) when (element(1, Trees) == trees2) ->
    #roots2{accounts = trie:root_hash(accounts, trees:accounts(Trees)),
           channels = trie:root_hash(channels, trees:channels(Trees)),
           existence = trie:root_hash(existence, trees:existence(Trees)),
           oracles = trie:root_hash(oracles, trees:oracles(Trees)),
           governance = trie:root_hash(governance, trees:governance(Trees)),
	   matched = trie:root_hash(matched, trees:matched(Trees)),
	   unmatched = trie:root_hash(unmatched, trees:unmatched(Trees))};
make_roots(Trees) when (element(1, Trees) == trees3) ->
    #roots3{accounts = trie:root_hash(accounts, trees:accounts(Trees)),
           channels = trie:root_hash(channels, trees:channels(Trees)),
           existence = trie:root_hash(existence, trees:existence(Trees)),
           oracles = trie:root_hash(oracles, trees:oracles(Trees)),
           governance = trie:root_hash(governance, trees:governance(Trees)),
	   matched = trie:root_hash(matched, trees:matched(Trees)),
	   unmatched = trie:root_hash(unmatched, trees:unmatched(Trees)),
           sub_accounts = trie:root_hash(sub_accounts, trees:sub_accounts(Trees)),
           contracts = trie:root_hash(contracts, trees:contracts(Trees)),
            trades = trie:root_hash(trades, trees:trades(Trees))};
make_roots(Trees) when (element(1, Trees) == trees4) ->
    #roots4{accounts = trie:root_hash(accounts, trees:accounts(Trees)),
           channels = trie:root_hash(channels, trees:channels(Trees)),
           existence = trie:root_hash(existence, trees:existence(Trees)),
           oracles = trie:root_hash(oracles, trees:oracles(Trees)),
           governance = trie:root_hash(governance, trees:governance(Trees)),
	   matched = trie:root_hash(matched, trees:matched(Trees)),
	   unmatched = trie:root_hash(unmatched, trees:unmatched(Trees)),
           sub_accounts = trie:root_hash(sub_accounts, trees:sub_accounts(Trees)),
           contracts = trie:root_hash(contracts, trees:contracts(Trees)),
            trades = trie:root_hash(trades, trees:trades(Trees)),
            markets = trie:root_hash(markets, trees:markets(Trees))};
make_roots(Trees) when (element(1, Trees) == trees5) ->
    %io:fwrite("block make roots 5\n"),
    %io:fwrite(integer_to_list(trees:accounts(Trees))),
    %io:fwrite("\n"),
    A = trie:root_hash(accounts, trees:accounts(Trees)),
    %io:fwrite("block make roots 5 1\n"),
 
    #roots5{accounts = A,
           channels = trie:root_hash(channels, trees:channels(Trees)),
           existence = trie:root_hash(existence, trees:existence(Trees)),
           oracles = trie:root_hash(oracles, trees:oracles(Trees)),
           governance = trie:root_hash(governance, trees:governance(Trees)),
	   matched = trie:root_hash(matched, trees:matched(Trees)),
	   unmatched = trie:root_hash(unmatched, trees:unmatched(Trees)),
           sub_accounts = trie:root_hash(sub_accounts, trees:sub_accounts(Trees)),
           contracts = trie:root_hash(contracts, trees:contracts(Trees)),
            trades = trie:root_hash(trades, trees:trades(Trees)),
            markets = trie:root_hash(markets, trees:markets(Trees)),
            receipts = trie:root_hash(receipts, trees:receipts(Trees)),
            stablecoins = trie:root_hash(stablecoins, trees:stablecoins(Trees))};
make_roots(V) when is_integer(V) ->
    %V is a pointer to a location in the verkle tree.
    trees2:root_hash(V).



roots_hash(X) when is_record(X, roots) ->%
    A = X#roots.accounts,%
    C = X#roots.channels,%
    E = X#roots.existence,%
    O = X#roots.oracles,%
    G = X#roots.governance,%
    Y = <<A/binary, C/binary, E/binary, O/binary, G/binary>>,%
    hash:doit(Y);%
roots_hash(X) when is_record(X, roots2) ->
    A = X#roots2.accounts,
    C = X#roots2.channels,
    E = X#roots2.existence,
    O = X#roots2.oracles,
    G = X#roots2.governance,
    M = X#roots2.matched,
    U = X#roots2.unmatched,
    Y = <<A/binary, C/binary, E/binary, O/binary, G/binary, M/binary, U/binary>>,
    hash:doit(Y);
roots_hash(X) when is_record(X, roots3) ->
    A = X#roots3.accounts,
    C = X#roots3.channels,
    E = X#roots3.existence,
    O = X#roots3.oracles,
    G = X#roots3.governance,
    M = X#roots3.matched,
    U = X#roots3.unmatched,
    SA = X#roots3.sub_accounts,
    Con = X#roots3.contracts,
    Tra = X#roots3.trades,
    Y = <<A/binary, C/binary, E/binary, O/binary, G/binary, M/binary, U/binary, SA/binary, Con/binary, Tra/binary>>,
    hash:doit(Y);
roots_hash(X) when is_record(X, roots4) ->
    A = X#roots4.accounts,
    C = X#roots4.channels,
    E = X#roots4.existence,
    O = X#roots4.oracles,
    G = X#roots4.governance,
    M = X#roots4.matched,
    U = X#roots4.unmatched,
    SA = X#roots4.sub_accounts,
    Con = X#roots4.contracts,
    Tra = X#roots4.trades,
    Markets = X#roots4.markets,
    Y = <<A/binary, C/binary, E/binary, O/binary, G/binary, M/binary, U/binary, SA/binary, Con/binary, Tra/binary, Markets/binary>>,
    hash:doit(Y);
roots_hash(X) when is_record(X, roots5) ->
    A = X#roots5.accounts,
    C = X#roots5.channels,
    E = X#roots5.existence,
    O = X#roots5.oracles,
    G = X#roots5.governance,
    M = X#roots5.matched,
    U = X#roots5.unmatched,
    SA = X#roots5.sub_accounts,
    Con = X#roots5.contracts,
    Tra = X#roots5.trades,
    Markets = X#roots5.markets,
    Receipts = X#roots5.receipts,
    Stablecoins = X#roots5.stablecoins,
    Y = <<A/binary, C/binary, E/binary, O/binary, G/binary, M/binary, U/binary, SA/binary, Con/binary, Tra/binary, Markets/binary, Receipts/binary, Stablecoins/binary>>,
    hash:doit(Y);
roots_hash(X = <<_:256>>) -> X.


    
guess_number_of_cpu_cores() ->
    case application:get_env(amoveo_core, test_mode) of
        {ok, true} -> 1;
        {ok, false} ->
            X = erlang:system_info(logical_processors_available),
            Y = if
                    X == unknown ->
                        % Happens on Mac OS X.
                        erlang:system_info(schedulers);
                    is_integer(X) -> 
                        %ubuntu
                        X;
                    true -> io:fwrite("number of CPU unknown, only using 1"), 1
                end,
            {ok, CoresToMine} = application:get_env(amoveo_core, cores_to_mine),
            min(Y, CoresToMine)
    end.
spawn_many(0, _) -> ok;
spawn_many(N, F) -> 
    spawn(F),
    spawn_many(N-1, F).
mine(Rounds) -> 
    %potential_block:save(),
    timer:sleep(10),
    Block = potential_block:read(),
    case Block of
	"" ->
	    timer:sleep(100),
	    mine(Rounds);
	_ ->
	    mine(Block, Rounds)
    end.
    %Block = potential_block:check(),
mine(Block, Rounds) ->
    %Cores = guess_number_of_cpu_cores(),
    Cores = 1, %slow down mining so I don't break the computer.
    mine(Block, Rounds, Cores).
mine(Block, Rounds, 1) ->
    case mine2(Block, Rounds) of
        false -> false;
        PBlock ->
%            io:fwrite("found a block\n"),
            Header = block_to_header(PBlock),
            headers:absorb([Header]),
            headers:absorb_with_block([Header]),
                        %block_absorber:save(PBlock),
            Hash = hash(Header),
            block_db3:write(PBlock, Hash),
            %block_organizer:add([PBlock])
            ok
                
                        %sync:start()
    end;
mine(Block, Rounds, Cores) ->
    F = fun() ->
                case mine2(Block, Rounds) of
                    false -> false;
                    PBlock ->
%                        io:fwrite("found a block\n"),
                        Header = block_to_header(PBlock),
                        headers:absorb([Header]),
			headers:absorb_with_block([Header]),
                        %block_absorber:save(PBlock),
                        Hash = hash(Header),
                        block_db3:write(PBlock, Hash),
                        %block_organizer:add([PBlock])
                        %sync:start()
                        ok
                end
        end,
    spawn_many(Cores-1, F),
    F().
mine2(Block, Times) ->
    PH = Block#block.prev_hash,
    ParentPlus = get_by_hash(PH),
    Trees = ParentPlus#block.trees,
    MineDiff = Block#block.difficulty,
    F2 = forks:get(2),
    Height = Block#block.height,
    Fork = if
	       F2 > Height -> 0;
	       true -> 1
	   end,
    case pow:pow(hash(Block), MineDiff, Times, Fork) of
        false -> false;
        Pow -> Block#block{nonce = pow:nonce(Pow)}
    end.
proofs_roots_match([], _) -> true;
proofs_roots_match({Proof, _Leaves}, R) 
  when is_binary(R) ->
    %proof is the serialized verlke proof.
    %r is the 32 byte root.
    %{true, _Leaves, _} = trees2:verify_proof(Proof),
    %io:fwrite({Proof}),
    %{ProofA, ProofB, ProofC} = Proof,
    %io:fwrite({length(ProofA), size(ProofB), length(ProofC)}),%fast proof is a tuple. {2, 32, 256}
    {Tree, _Commit, _Opening} = 
        if
            is_binary(Proof) ->
                %small proofs are serialized
                get_verkle:deserialize_proof(Proof);
            true -> 
                %fast proofs are not.
                Proof
        end,
    T1 = ed:decompress_point(hd(Tree)),
    R2 = stem_verkle:hash_point(T1),
    R2 == R;
%io:fwrite({Proof, R}),
%    1=2,
%    ok;
    %it is a verkle proof, and a verkle root. we need to check that they work together.
    
proofs_roots_match([P|T], R) when is_record(R, roots)->%
    Tree = proofs:tree(P),%
    Root = proofs:root(P),%
    Root = %
        case Tree of%
            oracle_bets -> Root;%
            orders -> Root;%
            accounts -> R#roots.accounts;%
            channels -> R#roots.channels;%
            existence -> R#roots.existence;%
            oracles -> R#roots.oracles;%
            governance -> R#roots.governance%
           end,%
    proofs_roots_match(T, R);%
proofs_roots_match([P|T], R) when is_record(R, roots2)->
    Tree = proofs:tree(P),
    Root = proofs:root(P),
    Root = case Tree of
	       accounts -> R#roots2.accounts;
	       channels -> R#roots2.channels;
	       existence -> R#roots2.existence;
	       oracles -> R#roots2.oracles;
	       governance -> R#roots2.governance;
	       matched -> R#roots2.matched;
	       unmatched -> R#roots2.unmatched
	   end,
    proofs_roots_match(T, R);
proofs_roots_match([P|T], R) when is_record(R, roots3)->
    Tree = proofs:tree(P),
    Root = proofs:root(P),
    Root = case Tree of
	       accounts -> R#roots3.accounts;
	       channels -> R#roots3.channels;
	       existence -> R#roots3.existence;
	       oracles -> R#roots3.oracles;
	       governance -> R#roots3.governance;
	       matched -> R#roots3.matched;
	       unmatched -> R#roots3.unmatched;
               sub_accounts -> R#roots3.sub_accounts;
               contracts -> R#roots3.contracts;
               trades -> R#roots3.trades
	   end,
    proofs_roots_match(T, R);
proofs_roots_match([P|T], R) when is_record(R, roots4)->
    Tree = proofs:tree(P),
    Root = proofs:root(P),
    Root = case Tree of
	       accounts -> R#roots4.accounts;
	       channels -> R#roots4.channels;
	       existence -> R#roots4.existence;
	       oracles -> R#roots4.oracles;
	       governance -> R#roots4.governance;
	       matched -> R#roots4.matched;
	       unmatched -> R#roots4.unmatched;
               sub_accounts -> R#roots4.sub_accounts;
               contracts -> R#roots4.contracts;
               trades -> R#roots4.trades;
               markets -> R#roots4.markets
	   end,
    proofs_roots_match(T, R);
proofs_roots_match([P|T], R) when is_record(R, roots5)->
    Tree = proofs:tree(P),
    Root = proofs:root(P),
    Root = case Tree of
	       accounts -> R#roots5.accounts;
	       channels -> R#roots5.channels;
	       existence -> R#roots5.existence;
	       oracles -> R#roots5.oracles;
	       governance -> R#roots5.governance;
	       matched -> R#roots5.matched;
	       unmatched -> R#roots5.unmatched;
               sub_accounts -> R#roots5.sub_accounts;
               contracts -> R#roots5.contracts;
               trades -> R#roots5.trades;
               markets -> R#roots5.markets;
               receipts -> R#roots5.receipts;
               stablecoins -> R#roots5.markets
	   end,
    proofs_roots_match(T, R);
proofs_roots_match(A, B) -> 
    io:fwrite({A, B}),
    1=2.
                            


verify(Block) ->
   %entire process of verifying the block in one place. Does not update the hard drive, does not calculate new tree root pointers.
    %does calculate the meta.
      %unused.
    X = check0(Block),
    B2 = Block#block{trees = X},
    OldBlock = get_by_hash(Block#block.prev_hash),
    {NewDict4, NewDict3, Dict, ProofTree} = 
        check3(OldBlock, B2), 
    Height = Block#block.height,
    OldTrees = OldBlock#block.trees,
    HeightCheck = Height,
    %Block2 = Block#block{trees = NewTrees3, meta = calculate_block_meta(Block, OldTrees, Dict, NewDict3)},
    Block2 = B2#block{meta = calculate_block_meta(Block, OldTrees, Dict, NewDict3)},
    TreesHash = Block2#block.trees_hash,
    %TreesHash = trees:root_hash(NewTrees3),
    {true, Block2}.

check0(Block) ->
    %This verifies the verkle proofs and the txs in ram. 
    %checks that the consensus state before processing the block matches what the previous headers says it should be.
    %is parallelizable
    %io:fwrite(" 0 block:check0 system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
    {ok, MTV} = application:get_env(
                  amoveo_core, minimum_to_verify),
    Height = Block#block.height,
    if
        (Height < MTV) -> 0;
        true ->
    Header = block_to_header(Block),
    BlockHash = hash(Header),
    case application:get_env(amoveo_core, assume_valid) of
        {ok, {Height, BlockHash}} ->
            %this is the block we are assuming is valid.
            ok;
        {ok, {Height, _}} ->
            io:fwrite("error, this block is not the one that we have assumed is valid for this height in the config file."),
            1=2;
        _ ->
            ok
    end,
    Facts = Block#block.proofs,
    {ok, Header} = headers:read(BlockHash),
    Roots = Block#block.roots,
    PrevStateHash = roots_hash(Roots),
    {ok, PrevHeader} = headers:read(Block#block.prev_hash),
    PrevStateHash = PrevHeader#header.trees_hash,
    %io:fwrite(" 2 block:check0 system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),

    %block.roots == prev_block.trees_hash

    Txs = Block#block.txs,
    RootSame = proofs_roots_match(Facts, Roots),
    if
        RootSame -> ok;
        true -> 
            io:fwrite({Roots, Facts}),
            1=2
    end,
    {Dict, ProofTree} = 
        if
            is_tuple(Facts) -> 
                %verkle version.
                {Proof, Leaves} = Facts,
                case application:get_env(amoveo_core, kind) of
                    {ok, "production"} ->
                        true = is_binary(Proof);
                    _ -> ok
                end,
 
                %io:fwrite(" 4 block:check0 system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
                {true, ProofTree0} = 
                    trees2:verify_proof(
                      Proof, Leaves, Height),
                %io:fwrite(" 6 block:check0 system memory " ++ integer_to_list(erlang:memory(binary)) ++ " \n"),
                false = is_integer(ProofTree0),
                Dict2 = proofs:facts_to_dict(
                         Facts, dict:new()),
                {Dict2, ProofTree0};
            %dict:store(proof, ProofTree, Dict2);
            is_list(Facts) -> 
                %merkle version.
                {proofs:facts_to_dict(
                  Facts, dict:new()), 0}
        end,
    %Dict = proofs:facts_to_dict(
    %{Facts, Leaves}, dict:new()),
    PrevHash = Block#block.prev_hash,
    _Pub = coinbase_tx:from(hd(Block#block.txs)),
    true = no_coinbase(tl(Block#block.txs)),
    NewDict = txs:digest(Txs, Dict, Height),
    SameLength = (length(dict:fetch_keys(Dict)) ==
                length(dict:fetch_keys(NewDict))),
    if
        SameLength -> ok;
        false and (Height == 13) -> 
            io:fwrite({lists:map(fun(X) -> dict:fetch(X, Dict) end, dict:fetch_keys(Dict)),
                       lists:map(fun(X) -> dict:fetch(X, NewDict) end, dict:fetch_keys(NewDict))});
        true ->
            io:fwrite({dict:fetch_keys(Dict),
                       dict:fetch_keys(NewDict)})
    end,
    {Dict, %consensus state proved by this block.
     NewDict, %consensus state after processing this block.
     ProofTree, %in verkle mode, this is the datastructure we can use to update the database. (maybe we use this to calculate the new root?)
     BlockHash}
    end.
            


check(Block) ->%This writes the result onto the hard drive database. This is non parallelizable.
    OldBlock = get_by_hash(Block#block.prev_hash),
    check2(OldBlock, Block).
check3(OldBlock, Block) ->
    %old block is this block's parent.

    %checks that checksums written on the block are correct. # of channels, # of accounts, marketcap, etc.
    %checks that the total number of coins before and after processing this block hasn't changed.
    %checks that the block isn't too big.
    %pays the block reward (which is weird, because we pay that in the coinbase tx as well.)


    %io:fwrite("block check3 0\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    Roots = Block#block.roots,
    {Dict, NewDict, ProofTree, BlockHash} = 
        Block#block.trees,
    %false = is_integer(ProofTree), %before the merkle update, this was an integer.
    %{Dict, NewDict} = check0(Block),
    %BlockHash = hash(Block),
    %io:fwrite("block check3 1\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    {ok, Header} = headers:read(BlockHash),
    Height = Block#block.height,
    F52 = forks:get(52),
    if
        Height > F52 ->
            false = is_integer(ProofTree);
        true ->
            true = is_integer(ProofTree)
    end,
    %io:fwrite("block check3 2\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    OldTrees = OldBlock#block.trees,
    Txs = Block#block.txs,
    Txs0 = tl(Txs),
    true = Block#block.channels_veo == OldBlock#block.channels_veo + deltaCV(Txs0, Dict),
    true = Block#block.live_channels == OldBlock#block.live_channels + many_live_channels(Txs0),
    true = Block#block.many_accounts == OldBlock#block.many_accounts + many_new_accounts(Txs0),
    true = Block#block.many_oracles == OldBlock#block.many_oracles + many_new_oracles(Txs0),
    true = Block#block.live_oracles == OldBlock#block.live_oracles + many_live_oracles(Txs0),
    %Governance = trees:governance(OldTrees),
    BlockSize = size(packer:pack(Txs)),
    F33 = forks:get(33),
    
    MaxBlockSize = if
                       Height > (F33+1) -> 
                           governance:dict_get_value(max_block_size, Dict, Height);
                       true -> none
                   end,
    %MaxBlockSize = governance:get_value(max_block_size, Governance),
    %io:fwrite("block check 3\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    ok = case BlockSize > MaxBlockSize of
	     true -> 
		 io:fwrite("error, this block is too big\n"),
		 bad;
	     false -> ok
    end,
    %BlockReward = governance:dict_get_value(block_reward, Dict, Height),
   % BlockReward_gov = trees:get(governance, block_reward, Dict, OldTrees),
   % BlockReward = governance:value(BlockReward_gov),
    BlockReward = governance:dict_get_value(block_reward, Dict, Height),
    
    
    

    %BlockReward = governance:get_value(block_reward, Governance),
    %io:fwrite("block check 4\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),

    %MarketCap = market_cap(OldBlock, BlockReward, Txs0, Dict, Height-1),
    %true = Block#block.market_cap == MarketCap,
    %io:fwrite("block check 5\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    B = (Height == forks:get(5)),
    NewDict2 = if
		B -> 
		    OQL = governance:new(governance:name2number(oracle_question_liquidity), constants:oracle_question_liquidity()),
		    governance:dict_write_new(OQL, NewDict);
		true -> NewDict
	    end,
    MinerAddress = element(2, hd(Txs)),
    FG6 = forks:get(6),
    FG9 = forks:get(9),
    MinerReward = miner_fees(Txs0),
    %io:fwrite("block check 5.0\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    NewDict3 = 
        if
            Height < FG6 -> NewDict2;
            Height < FG9 ->
%    MinerAccount = accounts:dict_get(MinerAddress, Dict),
                MinerAccount = accounts:dict_update_or_create(MinerAddress, NewDict2, MinerReward, none),
                accounts:dict_write(MinerAccount, NewDict2);
            true ->
                GovFees = gov_fees(Txs0, NewDict2, Height),
                MinerAccount2 = accounts:dict_update_or_create(MinerAddress, NewDict2, MinerReward - GovFees, none),
                accounts:dict_write(MinerAccount2, NewDict2)
        end,
    %io:fwrite("block check 5.1\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    F8 = forks:get(8),
    if
        Height > F8 ->
            if
                true -> ok;
                Height == 4 -> 
                    DKeys = dict:fetch_keys(NewDict3),
                    io:fwrite(lists:map(fun(X) -> {X, dict:fetch(X, Dict)} end, DKeys));
                true -> ok
            end,
            Diff0 = no_counterfeit(Dict, NewDict3, Txs0, Height),
            true = (Diff0 =< 0);
        true -> ok
    end,
    %io:fwrite("block check 5.2\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    NewDict4 = remove_repeats(NewDict3, Dict, Height),
    {NewDict4, NewDict3, Dict, ProofTree}.

root_hash_check(OldBlock, Block, NewDict4, ProofTree) ->
    TreesHash = Block#block.trees_hash,
    Height = Block#block.height,
    OldTrees = OldBlock#block.trees,
    false = is_integer(ProofTree),
    %NewTrees3 = 
    true = %use tree_data:verkle_dict_update_root TODO.
        trees_hash_maker(Height, OldTrees, NewDict4, 
                    ProofTree, TreesHash).
    %TreesHash2 = trees:root_hash(NewTrees3),
%    {TreesHash == TreesHash2,
%    Block#block{
%      trees = 0, 
%      meta = calculate_block_meta(
%               Block, OldTrees, Dict, NewDict3)}}.
    

check2(OldBlock, Block) ->
   %updates the merkle/verkle tree to store the new version of the consensus state. Checks that the resultant root hash matches what is written on the block. After the update it is using tree_data:verkle_dict_update_trie
    %calculates the meta data we store with blocks. (as decided in the config file.)

    {NewDict4, NewDict3, Dict, ProofTree} = 
        check3(OldBlock, Block), 
    Height = Block#block.height,
    OldTrees = OldBlock#block.trees,
    _Roots = Block#block.roots,
    %io:fwrite("block check 5.3\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),

    TreesHash = Block#block.trees_hash,

    HeightCheck = Height,
    %NewTrees3 = trees_maker(HeightCheck, OldTrees, NewDict4, TreesHash),
    %false = (Height == 39),
    NewTrees3 = trees_maker(HeightCheck, OldTrees, NewDict4, ProofTree, TreesHash),
    
    %{ok, PrevHeader} = headers:read(Header#header.prev_hash),
    %io:fwrite("block check 5.4\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    %PrevHashes2 = case Block#block.prev_hashes of
    %                  0 -> calculate_prev_hashes(PrevHeader);
    %                  X -> X
    %              end,
    %PrevHashes2 = calculate_prev_hashes(PrevHeader),
    %Block2 = Block#block{trees = NewTrees3, prev_hashes = PrevHashes2},
    %Block2 = Block#block{trees = NewTrees3, prev_hashes = PrevHashes2},

    Block2 = Block#block{trees = NewTrees3, meta = calculate_block_meta(Block, OldTrees, Dict, NewDict3)},
    %Block2 = Block#block{trees = NewTrees3, meta = <<>>},
    %TreesHash = trees:root_hash(Block2#block.trees),
    %TreesHash = trees:root_hash2(Block2#block.trees, Roots),
    %TreesHash = Header#header.trees_hash,
    %io:fwrite("block check 6\n"),
    %io:fwrite(packer:pack(erlang:timestamp())),
    %io:fwrite("\n"),
    %true = BlockHash == hash(Block2),
    %io:fwrite("pair before death \n"),
    %io:fwrite([NewTrees3, Roots]),
    %io:fwrite("\n"),
    %TreesHash = trees:root_hash2(NewTrees3, Roots),
    TreesHash2 = trees:root_hash(NewTrees3),
    if
        (TreesHash2 == TreesHash) -> ok;
        true -> io:fwrite(
                  {broken_tree_hash,
                   %Block2#block.meta,
                   orders:all(trees:orders(NewTrees3))
                   })
    end,
    {true, Block2}.
calculate_block_meta(Block, OldTrees, OldDict, NewDict) ->
    %json encoded with keys
    %every tx, including txid, type, quantities of veo being moved.
    case application:get_env(amoveo_core, block_meta) of
        {ok, true} ->
            H = Block#block.height,
            GM = governance:max(H),
            BlockPart = 
                case application:get_env(amoveo_core, block_meta_block) of
                    {ok, true} -> 
                        DR_gov = trees:get(governance, developer_reward, dict:new(), OldTrees),
                        DR = governance:value(DR_gov),
                        BR_gov = trees:get(governance, block_reward, dict:new(), OldTrees),
                        BR = governance:value(BR_gov),
                        DR1 = BR * DR div 10000,
                        [{block, {[
                                   {height, H}, 
                                   {developer_reward, DR1},
                                   {block_reward, BR},
                                   {diff, Block#block.difficulty},
                                   {prev_hash, base64:encode(Block#block.prev_hash)},
                                   {blockhash, base64:encode(hash(Block))},
                                   {time, Block#block.time},
                                   {market_cap, Block#block.market_cap}
                                  ]}}];
                    _ -> []
                end,
            TxPart = case application:get_env(amoveo_core, block_meta_txs) of
                         %{ok, true} -> [{txs, get_txs(Block#block.txs, OldTrees, OldDict, NewDict, H)}];
                         {ok, true} -> [{txs, get_txs(Block#block.txs, OldTrees, OldDict, NewDict, H)}];
                         _ -> []
                         end,
            GovPart = case application:get_env(amoveo_core, block_meta_governance) of
                         {ok, true} -> [{governance, {get_govs(OldTrees, GM, 1, [])}}];
                         _ -> []
                     end,
            BeforePart = case application:get_env(amoveo_core, block_meta_before) of
                             {ok, true} -> 
                                 [{before, dict_data(OldDict)}];
                             _ -> []
                         end,
            FollowingPart = case application:get_env(amoveo_core, block_meta_following) of
                             {ok, true} -> [{following, dict_data(NewDict)}];
                             _ -> []
                         end,
            J = {BlockPart ++ TxPart ++ GovPart ++ BeforePart ++ FollowingPart},
            jiffy:encode(J);
        _ -> X = <<>>,
             X
    end.
dict_data(D) ->
    K = dict:fetch_keys(D),
    dict_data2(K, D).
dict_data2([], _) -> [];
dict_data2([H = {existence, Key}|T], D) ->
    dict_data2(T, D);
dict_data2([H = {Type, Key}|T], D) ->
    %Y = case dict:fetch(H, D) of
    %        {B, _} -> B;
    %        B2 -> B2
    %    end,
    Y = case csc:read(H, D) of
            {empty, _, _} -> 0;
            {ok, _, Y3} -> Y3
        end,
    Z = case Y of
            0 -> 
                Key2 = if
                           is_binary(Key) -> [{key, base64:encode(Key)}];
                           is_integer(Key) -> [{key, Key}];
                           true -> 
                               {key, K1, K2} = Key,
                               [{account, base64:encode(K1)}, {oracle, base64:encode(K2)}]
                       end,
                Key2 ++ [{type, Type},{empty, true}];
            _ ->
                %X = Type:deserialize(Y),
                X = Y,
                unpack_tree_element(X)
        end,
    [{Z}|dict_data2(T, D)].
unpack_tree_element(X) ->
    case X of
        {<<B:520>>, N} -> [{type, unmatched_head2}, {pointer, base64:encode(<<B:520>>)}, {many, N}];
        _ ->
            case element(1, X) of
                gov -> [{type, gov},{id, governance:number2name(X#gov.id)},{value, X#gov.value},{lock, X#gov.lock}];
                acc -> [{type, account},{pubkey, base64:encode(X#acc.pubkey)},{balance, X#acc.balance},{nonce, X#acc.nonce}];
                oracle -> [{type, oracle},{oid, base64:encode(X#oracle.id)},{result, X#oracle.result},{starts, X#oracle.starts},{type, X#oracle.type},{done_timer, X#oracle.done_timer},{governance, X#oracle.governance},{governance_amount, X#oracle.governance_amount}];
                channel -> [{type, channel},{cid, base64:encode(X#channel.id)},{acc1, base64:encode(X#channel.acc1)}, {acc2, base64:encode(X#channel.acc2)}, {bal1, X#channel.bal1}, {bal2, X#channel.bal2}, {amount, X#channel.amount}, {nonce, X#channel.nonce}, {last_modified, X#channel.last_modified}, {delay, X#channel.delay}, {closed, X#channel.closed}];
                matched -> [{type, matched},{account, base64:encode(X#matched.account)}, {oracle, base64:encode(X#matched.oracle)}, {true, X#matched.true}, {false, X#matched.false}, {bad, X#matched.bad}];
                unmatched -> [{type, unmatched},{account, base64:encode(unmatched:account(X))}, {oracle, base64:encode(unmatched:oracle(X))}, {amount, unmatched:amount(X)},{pointer, base64:encode(unmatched:pointer(X))}];
                unmatched_head -> [{type, unmatched_head},{pointer, base64:encode(element(2, X))}, {many, element(3, X)}, {oid, base64:encode(element(4, X))}];
                oracle_bet -> [{type, oracle_bet},{id, base64:encode(oracle_bets:id(X))}, {true, oracle_bets:true(X)}, {false, oracle_bets:false(X)}, {bad, oracle_bets:bad(X)}];
                orders -> [{type, order}, {id, base64:encode(orders:aid(X))}, {amount, orders:amount(X)}, {pointer, base64:encode(element(4, X))}
                  ];
                orders_head -> [{type, orders_head},{pointer, base64:encode(element(2, X))}, {many, element(3, X)}, {oid, base64:encode(element(4, X))}];
                contract -> [{type, contract}, 
                             {code, base64:encode(X#contract.code)}, 
                             {many_types, X#contract.many_types}, 
                             {result, base64:encode(X#contract.result)}, 
                             {source, base64:encode(X#contract.source)}, 
                             {source_type, X#contract.source_type}];
                sub_acc -> [{type, sub_acc}];
                receipt -> [{type, receipt}, {id, base64:encode(receipts:id(X))}, {pubkey, base64:encode(receipts:pubkey(X))}, {tid, base64:encode(receipts:tid(X))}];
                trade -> [{type, trade}];
                Y -> 
                    %io:fwrite({failed_to_make_following, X}),
                    [];
                _ -> []
            end
    end.
get_txs_main(L, T, O, N, H) ->
    P1 = get_txs(L, T, O, N, H),
    DA = get_deleted_accounts(P1, []),
    %for each deleted account, check if that account gained value from either a spend, create_account_tx, from a channel_team_close tx
    ok.
    
get_deleted_accounts([], D) -> D;
get_deleted_accounts([H|T], D) ->
    %Type = json_get(type, H),
    Type = 0,
    if
        (Type == delete_acc_tx) ->
            get_deleted_accounts(T, [H|D]);
        true -> get_deleted_accounts(T, D)
    end.
get_txs([], _, _, _, _) -> [];
get_txs([H1|T], Trees, OldDict, NewDict, Height) ->
    H = case element(1, H1) of
             signed -> element(2, H1);
             _ -> H1
         end,
    Type = element(1, H),
    Txid = base64:encode(txs:txid(H1)),
    L = get_tx(H, Trees, OldDict, NewDict, Height),
    H2 = {[{type, Type},{txid, Txid}] ++ L},
    [H2|get_txs(T, Trees, OldDict, NewDict, Height)].
get_tx(T, _, _, _, _) when (element(1, T) == spend) ->
    [{to, base64:encode(T#spend.to)},
     {from, base64:encode(T#spend.from)},
     {amount, T#spend.amount},
     {fee, T#spend.fee}];
get_tx(T, _, _, _, _) when (element(1, T) == create_acc_tx) ->
    [{to, base64:encode(T#create_acc_tx.pubkey)},
     {from, base64:encode(T#create_acc_tx.from)},
     {amount, T#create_acc_tx.amount},
     {fee, T#create_acc_tx.fee}];
get_tx(T, _, _, _, _) when (element(1, T) == multi_tx) ->
    [{from, base64:encode(T#multi_tx.from)},
     {fee, T#multi_tx.fee},
     {txs, multi_tx_helper(T#multi_tx.txs)}
    ];
get_tx(T, _, _, _, _) when (element(1, T) == coinbase) ->
    [{to, base64:encode(T#coinbase.from)}
    ];
get_tx(T, _, _, _, _) when (element(1, T) == cs) ->
    [{from, base64:encode(T#cs.from)},
     {fee, T#cs.fee}
    ];
get_tx(T, _, _, _, _) when (element(1, T) == csc) ->
    [{from, base64:encode(T#csc.from)},
     {fee, T#csc.fee}
    ];
get_tx(T, Trees, _, _, _) when (element(1, T) == ctc) ->
    Channel = trees:get(channels, T#ctc.id, dict:new(), Trees),
    Amount1 = Channel#channel.bal1 + Channel#channel.amount,
    Amount2 = Channel#channel.bal2 - Channel#channel.amount,
    [{acc1, base64:encode(T#ctc.aid1)},
     {acc2, base64:encode(T#ctc.aid2)},
     {fee, T#ctc.fee},
     {cid, base64:encode(T#ctc.id)},
     {bal1, Amount1},
     {bal2, Amount2}
    ];
get_tx(T, _, _, _, _) when (element(1, T) == ctc2) ->
    [{acc1, base64:encode(T#ctc2.aid1)},
     {acc2, base64:encode(T#ctc2.aid2)},
     {fee, T#ctc2.fee},
     {cid, base64:encode(T#ctc2.id)},
     {bal1, T#ctc2.amount1},
     {bal2, T#ctc2.amount2}
    ];
get_tx(T, _, OldDict, _, _) when (element(1, T) == timeout) ->
    CID = T#timeout.cid,
    Channel = channels:dict_get(CID, OldDict),
    Aid1 = channels:acc1(Channel),
    Aid2 = channels:acc2(Channel),
    Amount = channels:amount(Channel),
    Bal1 = channels:bal1(Channel),
    Bal2 = channels:bal2(Channel),
    %if an account does not exist, we should give them nothing.
    [{cid, base64:encode(CID)},
     {fee, T#timeout.fee},
     {from, base64:encode(T#timeout.aid)},
     {acc1, base64:encode(Aid1)},
     {acc2, base64:encode(Aid2)},
     {bal1, Bal1 - Amount},
     {bal2, Bal2 + Amount}
    ];
get_tx(T, _, OldDict, _, _) when (element(1, T) == delete_acc_tx) ->
    From = T#delete_acc_tx.from,
    Acc = accounts:dict_get(From, OldDict),
    X = case Acc of
        empty -> [{amount, 0}];
        %error -> [{amount, 0}];
        _ -> [{amount, Acc#acc.balance}]
    end,
    X ++[{from, base64:encode(T#delete_acc_tx.from)},
         {fee, T#delete_acc_tx.fee},
         {to, base64:encode(T#delete_acc_tx.to)}
        ];
get_tx(T, _, _, _, _) when (element(1, T) == ex) ->
    [{from, base64:encode(T#ex.from)},
     {fee, T#ex.fee}
    ];
get_tx(T, _, _, _, _) when (element(1, T) == nc) ->
    [{acc1, base64:encode(T#nc.acc1)},
     {acc2, base64:encode(T#nc.acc2)},
     {fee, T#nc.fee},
     {bal1, T#nc.bal1},
     {bal2, T#nc.bal2},
     {delay, T#nc.delay},
     {cid, base64:encode(T#nc.id)}
    ];
get_tx(T, _, _, _, _) when (element(1, T) == nc_accept) ->
    NCO = signing:data(T#nc_accept.nc_offer),
    [{acc2, base64:encode(T#nc_accept.acc2)},
     {acc1, base64:encode(NCO#nc_offer.acc1)},
     {bal1, NCO#nc_offer.bal1},
     {bal2, NCO#nc_offer.bal2},
     {miner_commission, NCO#nc_offer.miner_commission},
     {delay, NCO#nc_offer.delay},
     {cid, base64:encode(NCO#nc_offer.id)},
     {fee, T#nc_accept.fee}
    ];
get_tx(T, _, _, NewDict, _) when (element(1, T) == oracle_bet) ->
    ID = oracle_bet_tx:id(T),
    Oracle = trees:get(oracles, ID, NewDict, ok),
    Type = case oracle_bet_tx:type(T) of
               1 -> true;
               2 -> false;
               3 -> bad_question
           end,
    [{from, base64:encode(oracle_bet_tx:from(T))},
     {amount, oracle_bet_tx:amount(T)},
     {bet_type, oracle_bet_tx:type(T)},
     {order_book_type, Oracle#oracle.type},
     {fee, oracle_bet_tx:fee(T)},
     {done_timer, Oracle#oracle.done_timer},
     {oracle_id, base64:encode(ID)}
    ];
get_tx(T, _, OldDict, _NewDict, _) when (element(1, T) == oracle_close) ->
    ID = T#oracle_close.oracle_id,
    Oracle = trees:get(oracles, ID, OldDict, ok),
    [{from, base64:encode(T#oracle_close.from)},
     {fee, T#oracle_close.fee},
     {oracle_id, base64:encode(ID)},
     {done_timer, Oracle#oracle.done_timer},
     {result, Oracle#oracle.result}
    ];
get_tx(T, _, _, NewDict, _) when (element(1, T) == oracle_new) ->
    ID = T#oracle_new.id,
    Oracle = trees:get(oracles, ID, NewDict, ok),
    [{from, base64:encode(T#oracle_new.from)},
     {fee, T#oracle_new.fee},
     {governance, T#oracle_new.governance},
     {governance_amount, T#oracle_new.governance_amount},
     {start, T#oracle_new.start},
     {done_timer, Oracle#oracle.done_timer},
     {question, base64:encode(T#oracle_new.question)},
     {oracle_id, base64:encode(ID)}];
get_tx(T, _, OldDict, _, Height) when (element(1, T) == unmatched) ->
    From = oracle_unmatched_tx:from(T),
    OID = oracle_unmatched_tx:oracle_id(T),
    F10 = Height > forks:get(10),
    UMT = if%
	      F10  -> unmatched;
	      true -> orders%
	  end,%
    Order = UMT:dict_get({key, From, OID}, OldDict),
    Amount = UMT:amount(Order),
    [{from, base64:encode(From)},
     {fee, oracle_unmatched_tx:fee(T)},
     {oracle_id, base64:encode(OID)},
     {amount, Amount}
    ];
get_tx(T, _, OldDict, NewDict, Height) when (element(1, T) == oracle_winnings) ->
    From = oracle_winnings_tx:from(T),
    OID = oracle_winnings_tx:oracle_id(T),
    F10 = Height > forks:get(10),
    UMT = if
	      F10  -> matched;
	      true -> oracle_bets
	  end,
    Bet = UMT:dict_get({key, From, OID}, OldDict),
    Oracle = trees:get(oracles, OID, NewDict, ok),
    Result = Oracle#oracle.result,
    Reward = UMT:reward(Bet, Result, Height),
    [{from, base64:encode(T#oracle_winnings.from)},
     {fee, T#oracle_winnings.fee},
     {oracle_id, base64:encode(T#oracle_winnings.oracle_id)},
     {amount, Reward}
    ];
get_tx(_, _, _, _, _) -> [].
multi_tx_helper([]) -> [];
multi_tx_helper([H|T]) ->
    Type = element(1, H),
    %M = txs:key2module(Type),
    X = case Type of
            spend -> mth_spend(H);
            create_acc_tx -> mth_spend(H);
            _ -> {[{type, Type}]}
        end,
    [X|multi_tx_helper(T)].
mth_spend(H) ->
    Type = element(1, H),
    From = element(2, H),
    To = element(5, H),
    Amount = element(6, H),
    {[{type, Type},
      {to, base64:encode(To)},
      {amount, Amount}
     ]}.
get_govs(_, M, M, X) -> X;
get_govs(T, M, N = 2, X) ->
    G1= trees:get(governance, N, dict:new(), T),
    G = governance:value(G1),
    H = {governance:number2name(N), G / 10000},
    get_govs(T, M, N+1, [H|X]);
get_govs(T, M, N, X) ->
    G1= trees:get(governance, N, dict:new(), T),
    G = governance:value(G1),
    H = {governance:number2name(N), G},
    get_govs(T, M, N+1, [H|X]).
%    <<>>.

%this stuff might be useful for making it into a light node.
%setup_tree(Empty, Start, Path, Type) ->
%    case Start of
%        Empty ->
%            Hashes = hd(lists:reverse(Path)),
%            Stem = stem:make(Hashes, Type),
%            trie:new_trie(Type, Stem);
%        X -> X
%    end.
%ftt2(Fact, Trees) ->
%    Type = proofs:tree(Fact),
%    case Type of
%        orders ->
%            {key, _Pubkey, OID} = proofs:key(Fact),
%            Oracles = trees:oracles(Trees),
%            Path = proofs:path(Fact),
%            {_, Oracle, _} = oracles:get(OID, Oracles),
%            case Oracle of 
%                empty -> 
%                    Trees;
%                _ -> 
%                    Orders = Oracle#oracle.orders,
%                    Orders2 = setup_tree(0, Orders, Path, Type),
%                    Orders3 = trees:restore(Orders2, Fact, 0),
%                    Oracle2 = oracles:set_orders(Oracle, Orders3),
%                    Oracles2 = oracles:write(Oracle2, Oracles),
%                    trees:update_oracles(Trees, Oracles2)
%            end;
%        oracle_bets -> 
%            {key, Pubkey, _OID} = proofs:key(Fact),
%            Path = proofs:path(Fact),
%            Accounts = trees:accounts(Trees),
%            {_, Account, _} = accounts:get(Pubkey, Accounts),
%            Bets = Account#acc.bets,
%            Bets2 = setup_tree(0, Bets, Path, Type),
%            Bets3 = trees:restore(Bets2, Fact, 0),
%            Account2 = accounts:update_bets(Account, Bets3),
%            Accounts2 = accounts:write(Account2, Accounts),
%            trees:update_accounts(Trees, Accounts2);
%        _ ->
%            Path = proofs:path(Fact),
%            Tree = setup_tree(empty, trees:Type(Trees), Path, Type),
%            Tree2 = trees:restore(Tree, Fact, 0),
%            Update = list_to_atom("update_" ++ atom_to_list(Type)),
%            trees:Update(Trees, Tree2)
%    end.
no_coinbase([]) -> true;
no_coinbase([STx|T]) ->
    Tx = signing:data(STx),
    Type = element(1, Tx),
    false = Type == coinbase,
    no_coinbase(T).

initialize_chain() -> 
    %only run genesis maker once, or else it corrupts the database.
    %{ok, L} = file:list_dir("blocks"),
    %B = length(L) < 1,
    %B = (element(2, headers:top_with_block()) == 0),
    B = get_by_height(0) == error,
    %B = true,
    {GB, Bool} = if
                     B -> G = genesis_maker(),
                          %block_absorber:do_save(G, GH),
                          {G, true};
                     true -> {get_by_height(0), false}
                 end,
    Header0 = block_to_header(GB),
    GH = block:hash(Header0),
    if
        Bool -> 
            block_absorber:do_save(GB, GH);
        true -> ok
    end,
    gen_server:call(headers, {add, GH, Header0, 1}),
    gen_server:call(headers, {add_with_block, block:hash(Header0), Header0}),
    Header0.

gov_fees([], _, _) -> 0;
gov_fees([Tx|T], Dict, Height) ->
    C = signing:data(Tx),
    Type = element(1, C),
    A = case Type of
	    multi_tx -> gov_fees2(C#multi_tx.txs, Dict, Height);
            contract_timeout_tx2 -> 0;
	    _ -> 
                X = governance:dict_get_value(Type, Dict, Height),
                F16 = forks:get(16),
                if
                    ((Type == timeout) and (Height > F16)) -> -X;
                    true -> X
                end
	end,
    A + gov_fees(T, Dict, Height).
gov_fees2([], _, _) -> 0;
gov_fees2([H|T], Dict, Height) ->
    Type = element(1, H),
    F47_activated = forks:get(47) < Height,
    A = if
            (Type == contract_timeout_tx2) ->
                0;
            (F47_activated and (Type == contract_evidence_tx)) -> 
                CEF = governance:dict_get_value(Type, Dict, Height),
                Contract = H#contract_evidence_tx.contract,
                Evidence = H#contract_evidence_tx.evidence,
                Prove = H#contract_evidence_tx.prove,
                S = size(Evidence) + size(Contract) + (16 * 32 * 5 * length(Prove)),
                CEF + (S * CEF div 5000);
            (F47_activated and (Type == oracle_new)) -> 
                S = size(H#oracle_new.question),
                ONF = governance:dict_get_value(Type, Dict, Height),
                ONF + (S * ONF div 5000);
            true ->
        
            governance:dict_get_value(Type, Dict, Height)
        end,
    A + gov_fees2(T, Dict, Height).
    
deltaCV([], _) -> 0;%calculate change in total amount of VEO stored in channels.
deltaCV([Tx|T], Dict) ->
    C = signing:data(Tx),
    A = case element(1, C) of
	    nc -> new_channel_tx:bal1(C) + new_channel_tx:bal2(C);
	    ctc2 -> 
		ID = channel_team_close_tx2:id(C),
		OldChannel = channels:dict_get(ID, Dict),
		%io:fwrite(packer:pack(OldChannel)),
		Bal1 = channels:bal1(OldChannel),
		Bal2 = channels:bal2(OldChannel),
		-(Bal1 + Bal2);
	    ctc -> 
		ID = channel_team_close_tx:id(C),
		OldChannel = channels:dict_get(ID, Dict),
		%io:fwrite(packer:pack(OldChannel)),
		Bal1 = channels:bal1(OldChannel),
		Bal2 = channels:bal2(OldChannel),
		-(Bal1 + Bal2);
	    timeout -> 
		ID = channel_timeout_tx:cid(C),
		OldChannel = channels:dict_get(ID, Dict),
		Bal1 = channels:bal1(OldChannel),
		Bal2 = channels:bal2(OldChannel),
		-(Bal1 + Bal2);
	    _ -> 0
	end,
    A + deltaCV(T, Dict).
many_live_channels([]) -> 0;
many_live_channels([Tx|T]) ->
    C = signing:data(Tx),
    A = case element(1, C) of
	    nc -> 1;
	    ctc -> -1;
	    ctc2 -> -1;
	    timeout -> -1;
	    _ -> 0
	end,
    A + many_live_channels(T).
many_new_accounts([]) -> 0;
many_new_accounts([Tx|T]) ->
    C = signing:data(Tx),
    A = case element(1, C) of
	    create_acc_tx -> 1;
	    delete_acc_tx -> -1;
	    _ -> 0
	end,
    A + many_new_accounts(T).
many_new_oracles([]) -> 0;
many_new_oracles([Tx|T]) ->
    C = signing:data(Tx),
    A = case element(1, C) of
	    oracle_new -> 1;
	    _ -> 0
	end,
    A + many_new_oracles(T).
many_live_oracles([]) -> 0;
many_live_oracles([Tx|T]) ->
    C = signing:data(Tx),
    A = case element(1, C) of
	    oracle_new -> 1;
	    oracle_close -> -1;
	    _ -> 0
	end,
    A + many_live_oracles(T).

all_mined_by(Address) ->
    B = top(),
    Height = B#block.height,
    bmb_helper(Address, [], hash(B), Height - 1).
bmb_helper(Address, Out, Hash, 0) -> Out;
bmb_helper(Address, Out, Hash, Height) ->
    B = block:get_by_hash(Hash),
    Txs = B#block.txs,
    CB = hd(Txs),
    A2 = element(2, CB),
    Out2 = if
	       Address == A2 -> [Height|Out];
	       true -> Out
	   end,
    PH = B#block.prev_hash,
    bmb_helper(Address, Out2, PH, Height - 1).
time_mining(X) -> time_mining(0, X, []).
time_mining(S, [], X) -> tl(lists:reverse(X));
time_mining(S, Heights, Outs) ->
    B = block:get_by_height(hd(Heights)),
    T = B#block.time,
    T2 = T - S,
    time_mining(T, tl(Heights), [(T-S)|Outs]).

period_estimate() ->
    period_estimate(top()).
period_estimate(T) when is_integer(T) ->
    period_estimate(get_by_height(T));
period_estimate(T) ->
    %estimates seconds per block
    H = T#block.height,
    true = H > 21,
    X = get_by_height(H-20),
    Time1 = X#block.time,
    Time2 = T#block.time,
    (Time2 - Time1) div 200.
hashes_per_block() ->
    hashes_per_block(top()).
hashes_per_block(B) ->
    D = B#block.difficulty,
    diff2hashes(D) div 1000000000.
hashrate_estimate() ->
    hashrate_estimate(top()).
hashrate_estimate(T) when is_integer(T) ->
    hashrate_estimate(get_by_height(T));
hashrate_estimate(T) ->
    %estimates hashes per second
    D = T#block.difficulty,
    Hashes = diff2hashes(D),
    X = Hashes / period_estimate(T) / 1000000000,
    %io:fwrite("in gigahashes per second "),
    %io:fwrite(integer_to_list(round(10*X))),
    %io:fwrite("\n"),
    round(X).
diff2hashes(D) ->
    A = D div 256,
    B = D rem 256,
    exponent(2, A) * (256 + B) div 256.
exponent(_, 0) -> 1;
exponent(A, 1) -> A;
exponent(A, N) when N rem 2 == 0 -> 
    exponent(A*A, N div 2);
exponent(A, N) -> 
    A*exponent(A, N-1).

count(_, [], N) -> N;
count(Type, [H|T], N) ->
    Type2 = element(1, element(2, H)),
    if
        Type == Type2 -> count(Type, T, N+1);
        true -> count(Type, T, N)
    end.
many_close_oracles([], N) -> N;
many_close_oracles([{signed, Tx, _, _}|
                   T], N)
  when is_record(Tx, oracle_close) ->
    many_close_oracles(T, N+1);
many_close_oracles([{signed, Tx, _, _}|
                   T], N) 
  when is_record(Tx, multi_tx) ->
    M = mco_multi(Tx#multi_tx.txs, 0),
    many_close_oracles(T, N+M);
many_close_oracles([_|T], N) ->
    many_close_oracles(T, N).
mco_multi([], M) -> M;
mco_multi([Tx|T], M) 
  when is_record(Tx, oracle_close) ->
    mco_multi(T, M+1);
mco_multi([_|T], M) ->
    mco_multi(T, M).



            
no_counterfeit(Old, New, Txs, Height) ->
    %times it was outside expected range.
   %height 2014; diff is - 34434339
    %height 2017; diff is  49695764
    %height 3044; diff is  15869694
   %height 4133; diff is - 34130307
    %height 4137; diff is  39695764
    %height 5141; diff is   5869694
   %height 15583; diff is  33847882
   %height 21097; diff is  99847882
   %height 22692; diff is  12210869
   %height 22703; diff is  16847882
   %height 22715; diff is  17847882
   %height 23047; diff is  99847882
   %height 24897; diff is  65924643
  %height 30166; diff is 1 00000000
   %height 30334; diff is  11051240
  %height 32271; diff is 1 00000000
  %height 32528; diff is 3 00000000
%height 33178; diff is 258 00000000
%height 34116; diff is 121 00000000
 %height 34556; diff is 57 99999999
 %height 34587; diff is 41 09394235
 %height 34626; diff is -3 92481529
 %height 34627; diff is 13 91619411
 %height 34680; diff is 67 16797942
%height 34681; diff is -17 11257793
%height 34905; diff is 598 01505175

    
    OK = dict:fetch_keys(Old),
    NK = dict:fetch_keys(New),
    lists:map(fun(Key) -> 
                      {ok, V} = dict:find(Key, Old),
                      case Key of
                          {_, _} -> ok;
                          _ -> io:fwrite({V, Key, OK})
                      end,
                      if
                          not(is_record(V, consensus_state)) -> io:fwrite({Key, V, dict:fetch_keys(Old)});
                          true -> ok
                      end
              end, OK),
    OA = sum_amounts(OK, Old, Old, Height),
    NA = sum_amounts(NK, New, Old, Height),
    BR = governance:dict_get_value(block_reward, Old, Height),
    %io:fwrite("block reward "),
    %io:fwrite(integer_to_list(BR)),
    %io:fwrite("\n"),
    DR = governance:dict_get_value(developer_reward, Old, Height),
    DR1 = (BR * DR div 10000),
    F49 = forks:get(49),
    DR2 = case Height of
              F49 -> DR1 + 4958336858 + (50 * 60657);
            _ -> DR1
        end,
    %io:fwrite("block reward "),
    %io:fwrite(integer_to_list(BR + (BR * DR div 10000))),
    %BlockReward = BR + (BR * DR div 10000),
    BlockReward = BR + DR2,
    %io:fwrite("; "),
    CloseOracles = many_close_oracles(Txs, 0),
    %CloseOracles = count(oracle_close, Txs, 0),
    %io:fwrite("close oracles are "),
    %io:fwrite(integer_to_list(CloseOracles)),
    %io:fwrite("; "),
    OIL = governance:dict_get_value(oracle_initial_liquidity, Old, Height),% div 2;
    OCA = if
              ((CloseOracles > 0) and (is_integer(OIL)))->
                  OIL div 2;
              (CloseOracles > 0) ->
                  OQL = governance:dict_get_value(oracle_question_liquidity, Old, Height),
                  OQL div 2;
              true -> 0
          end,
    Diff = (NA - OA) - (OCA * CloseOracles) - BlockReward,
    BurnLimit = 
        case application:get_env(amoveo_core, test_mode) of
            %{ok, true} ->   -1;
            {ok, true} ->  -50000000;
            {ok, false} -> -50000000
        end,
    if
        ((Diff > 0) or (Diff < BurnLimit))->
        %false ->
            io:fwrite("Accounting error. Number of coins in doesn't equal number of coins out.\nheight "),
            io:fwrite(integer_to_list(Height)),
            io:fwrite("; diff is "),
            io:fwrite(integer_to_list(Diff)),
            io:fwrite("\n"),
            case application:get_env(amoveo_core, test_mode) of
                {ok, true} -> 
                    if
                        Diff > 0 -> io:fwrite("error, counterfeiting\n"),
                                    0=1;
                        true -> 1=2
                                
                    end;
                _ -> 
                    io:fwrite({OK, NK})
            end,
            %io:fwrite(packer:pack([0, NK, OK])),
            io:fwrite("\n");
        true -> ok
    end,
    Diff.

%    true = (Diff =< 0),
%    ok.
sum_amounts([], _, _, _) -> 
    %io:fwrite("sum amount finish\n"),
    0;
%sum_amounts([R|T], Dict, OldDict, Height) 
%  when is_binary(R) ->
%    X = case csc:read2(R, Dict) of
%            {ok, #consensus_state{empty = true}} ->
%                0;
%            %{empty, Type, UnhashedKey} -> 0;
%            {ok, #consensus_state{
%               type = Type, val = V, 
%               unhashed_key = UnHashedKey}} ->
%                %{ok, Type, V} -> 
%                V2 = case V of
%                         {A, _} -> A;
%                         B -> B
%                     end,
%                sum_amounts_helper(
%                  Type, V2, Dict, 
%                  OldDict, UnHashedKey)
%        end,
%    X + sum_amounts(T, Dict, OldDict, Height);
sum_amounts([{oracles, _}|T], Dict, OldDict, Height) ->
    sum_amounts(T, Dict, OldDict, Height);
sum_amounts([{existence, _}|T], Dict, Old, Height) ->
    sum_amounts(T, Dict, Old, Height);
sum_amounts([{governance, _}|T], Dict, Old, Height) ->
    sum_amounts(T, Dict, Old, Height);
sum_amounts([proof|T], Dict, Old, Height) ->
    sum_amounts(T, Dict, Old, Height);
sum_amounts([{unmatched, {key, <<1:520>>}}|T], Dict, Old, Height) ->
    io:fwrite("unmatched head possibly? \n"),
    %io:fwrite({dict:fetch_keys(Dict)}),
    sum_amounts(T, Dict, Old, Height);
sum_amounts([{empty, A}|T], Dict, Old, Height) ->
    sum_amounts(T, Dict, Old, Height);
sum_amounts([{Kind, A}|T], Dict, Old, Height) ->
    X = Kind:dict_get(A, Dict, Height),
    B = sum_amounts_helper(Kind, X, Dict, Old, A),
    if
        false ->
            io:fwrite("sum amount, type: "),
            io:fwrite(Kind),
            io:fwrite(" key: "),
            io:fwrite(packer:pack(A)),
            io:fwrite(" "),
            io:fwrite(integer_to_list(B)),
            io:fwrite("\n");
        true -> ok
    end,
    B + sum_amounts(T, Dict, Old, Height).
%sum_amounts_helper(_, error, _, _, _) -> 0;
sum_amounts_helper(_, empty, _, _, _) -> 0;
sum_amounts_helper(governance, _, _, _, _) ->
    0;
sum_amounts_helper(receipts, Acc, Dict, _, _) ->
    0;
sum_amounts_helper(sub_accounts, Acc, Dict, _, _) ->
    0;
sum_amounts_helper(trades, Acc, Dict, _, _) -> 0;
sum_amounts_helper(stablecoins, Acc, Dict, _, _) -> 0;
sum_amounts_helper(markets, M, Dict, _, _) ->
    #market{
              cid1 = CID1,
              cid2 = CID2,
              amount1 = Amount1,
              amount2 = Amount2
            } = M,
    A1 = case CID1 of
             <<0:256>> -> Amount1;
             _ -> 0
         end,
    A2 = case CID2 of
             <<0:256>> -> Amount2;
             _ -> 0
         end,
    A1 + A2;
sum_amounts_helper(contracts, Acc, Dict, _, _) ->
    case Acc#contract.source of
        <<0:256>> ->
            Acc#contract.volume;
        _ -> 0
    end;
sum_amounts_helper(accounts, Acc, Dict, _, _) ->
    %io:fwrite({Acc}),
    if
        not(is_record(Acc, acc)) ->
            io:fwrite({size(Acc), Acc});
        true -> ok
    end,
    Acc#acc.balance;
sum_amounts_helper(channels, Chan, Dict, _, _) ->
    case channels:closed(Chan) of
        0 ->
            channels:bal1(Chan) + 
                channels:bal2(Chan);
        1 -> 0
    end;
sum_amounts_helper(oracle_bets, OB, Dict, OldDict, Key) ->%
    {key, Pub, OID} = Key,%
    Oracle = oracles:dict_get(OID, OldDict),%
    R = Oracle#oracle.result,%
    T = oracle_bets:true(OB),%
    F = oracle_bets:false(OB),%
    B = oracle_bets:bad(OB),%
    N = case R of%
            0 -> ((T + F + B) div 2);%
            1 -> T;%
            2 -> F;%
            3 -> B%
        end;%
sum_amounts_helper(orders, Ord, Dict, _, Key) ->%
    PS = constants:pubkey_size() * 8,%
    case Key of%
        {key, <<1:PS>>, _} -> 0;%
        _ -> orders:amount(Ord)%
    end;%
sum_amounts_helper(unmatched, UM, _Dict, _, Key) ->
    PS = constants:pubkey_size() * 8,
    case Key of
        {key, <<1:PS>>, _} -> 0;
        _ -> 
            unmatched:amount(UM)
    end;
sum_amounts_helper(matched, M, _Dict, OldDict, Key) ->
    {key, Pub, OID} = Key,
    Oracle = oracles:dict_get(OID, OldDict),
    R = case Oracle of
            empty -> 0;
            error -> 0;
            _ ->
                Oracle#oracle.result
        end,
    T = matched:true(M),
    F = matched:false(M),
    B = matched:bad(M),
    %((T+F+B) div 2).
    N = case R of
            0 -> ((T+F+B) div 2);
            1 -> T;
            2 -> F;
            3 -> B
        end;
sum_amounts_helper(jobs, Job, _dict, _, _) ->
    R = Job#job.balance,
    if
        not(is_integer(R)) -> io:fwrite(Job);
        true -> R
    end;


%how does liquidity move through.
% when you create a futarchy, liquidity goes from the account to the new futarchy. lmsr:veo_in_market/3 for the 2 markets inside the futarchy. This is the liquidity for the market.

% when a bet is made, it can only increase the number of shares in the lmsr, so betting only increases the veo stored in the market market. 

% when the futarchy decision is finalized, in the reverted market the creator gains lmsr:veo_in_market. In the market that is not reverted, they get paid in a mixture of veo and a subcurrency in the new binary market.

sum_amounts_helper(futarchy, Futarchy, _dict, _, _) ->
    #futarchy{
               fid = FID,
               liquidity_true = Bt,
               liquidity_false = Bf,
               shares_true_yes = STY,
               shares_true_no = STN,
               shares_false_yes = SFY,
               shares_false_no = SFN,
               active = A
             } = Futarchy,
    R = case A of
            1 ->
                lmsr:veo_in_market(Bt, STY, STN) +
                    lmsr:veo_in_market(Bf, SFY, SFN);
            0 ->
                Bf = STY,
                Bf = STN,
                Bf = SFY,
                Bf = SFN,
                Bf = 0,
                true = Bt >= 0,
                %we removed the liquidity from the reverted market, because that money went to the creator.
                %we removed everything from the unreverted market, because that went to the contract.
                
                %shares in the reverted market are still around, they will get withdrawn in futarchy_matched/futarchy_unmatched txs. The value of this is stored in Bt.
                Bt
        end,
%    io:fwrite("block sum amounts helper futarchy has: "),
%    io:fwrite(base64:encode(FID)),
%    io:fwrite(" : "),
%    io:fwrite(integer_to_list(R)),
%    io:fwrite("\n"),
    R;
sum_amounts_helper(futarchy_unmatched, FU, _dict, 
                   _, _) ->
    #futarchy_unmatched{
                    revert_amount = RA
                   } = FU,
    RA;
sum_amounts_helper(futarchy_matched, FM, _dict, 
                  _, _) ->
    #futarchy_matched{
                    revert_amount = RA
                   } = FM,
    %as long as subcurrencies have value of 0, then futarchy_matched also need to have a value of 0. Since they get converted into subcurrency.
    0.%sometimes we convert futarchy_matched into the share type in the binary market that ends up winning. For accounting, we need to remove the futarchy_matched, and add it's liquidity checksum to the contract.
%we don't want the contract's liquidity to go negative, 
    %RA.

remove_repeats(New, Old, Height) ->
    %todo. the old one has "proof" in it. we don't want to lose it in the new one.
    Keys = dict:fetch_keys(New),
    F10 = forks:get(10),
    Old2 = if
               Height =< F10 -> oracle_bet_order_scanner(dict:fetch_keys(New), New, Old);%check for orders and oracle bets. if there are, then remove them from old
               true -> Old
           end,
    remove_repeats2(New, Old2, Keys).
oracle_bet_order_scanner([], _, Old) -> Old;
oracle_bet_order_scanner([{orders, {key, _Pub, OID}}|T], New, Old) ->
    Old2 = dict:erase({oracles, OID}, Old),
    oracle_bet_order_scanner(T, New, Old2);
oracle_bet_order_scanner([{oracle_bets, {key, Pub, _OID}}|T], New, Old) ->
    Old2 = dict:erase({accounts, Pub}, Old),
    oracle_bet_order_scanner(T, New, Old2);
oracle_bet_order_scanner([_|T], New, Old) ->
    oracle_bet_order_scanner(T, New, Old).
    
remove_repeats2(New, _, []) -> New;
remove_repeats2(New, Old, [H|T]) ->
    N = dict:fetch(H, New),
    O = case dict:find(H, Old) of
            error -> error;
            {ok, X} -> X
        end,
    New2 = if
               (N == O) -> 
                   dict:erase(H, New);
               true  -> 
                   New
           end,
    remove_repeats2(New2, Old, T).
            
	    
test() ->
    test(1).
test(1) ->
    Header0 = headers:top(),
    Block0 = get_by_hash(Header0),
    Trees = Block0#block.trees,
    make_roots(Trees),
    Pub = keys:pubkey(),
    Block1 = make(Header0, [], Trees, Pub),
    WBlock10 = mine2(Block1, 10),
    Header1 = block_to_header(WBlock10),
    io:fwrite("header 1 "),
    io:fwrite(packer:pack(Header1)),
    io:fwrite("\n"),
    headers:absorb([Header1]),
    headers:absorb_with_block([Header1]),
    H1 = hash(Header1),
    H1 = hash(WBlock10),
    {ok, _} = headers:read(H1),
    block_organizer:add([WBlock10]),
    timer:sleep(400),
    WBlock11 = get_by_hash(H1),
    WBlock12 = get_by_height_in_chain(1, Header1),
    io:fwrite(packer:pack(WBlock12)),
    io:fwrite("\n"),
    io:fwrite(packer:pack(WBlock11)),
    io:fwrite("\n"),
    WBlock11 = WBlock12,
    io:fwrite(packer:pack(WBlock10)),
    io:fwrite("\n"),
    WBlock13 = WBlock11#block{trees = WBlock10#block.trees, meta = <<>>},
    %io:fwrite(packer:pack([WBlock13, WBlock10])),
    WBlock13 = WBlock10,
    success;
test(2) ->
    {_, _, Proofs} = accounts:get(keys:pubkey(), 1),
    _Proof = hd(Proofs).

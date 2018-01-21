-module(block).
-export([block_to_header/1, get_by_height_in_chain/2,
         get_by_height/1, hash/1, get_by_hash/1, 
         initialize_chain/0, make/4,
         mine/1, mine/2, mine2/2, check/1, 
         top/0, genesis_maker/0, height/0,
         dict_update_trie/2, test/0]).
%Read about why there are so many proofs in each block in docs/design/light_nodes.md
-include("../../records.hrl").
-record(roots, {accounts, channels, existence, oracles, governance}).

tx_hash(T) -> hash:doit(T).
proof_hash(P) -> hash:doit(P).
merkelize_thing(X) when is_binary(X) -> X;
merkelize_thing(X) ->
    T = element(1, X),
    case T of
        proof -> proof_hash(X);
        _ -> tx_hash(X)
    end.
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
    headers:make_header(
      B#block.prev_hash,
      B#block.height,
      B#block.time,
      B#block.version,
      B#block.trees_hash,
      txs_proofs_hash(B#block.txs, B#block.proofs),
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
    case NHeight < 1 of
        true ->
            list_to_tuple([prev_hashes|lists:reverse([PH|Hashes])]);
        false ->
            B = get_by_height_in_chain(NHeight, PH),
            calculate_prev_hashes([hash(B)|[PH|Hashes]], NHeight, N*2)
    end.
get_by_hash(H) ->
    Hash = hash(H),
    BlockFile = ae_utils:binary_to_file_path(blocks, Hash),
    case db:read(BlockFile) of
        [] -> empty;
        Block -> binary_to_term(zlib:uncompress(Block))
    end.
top() -> top(headers:top()).
top(Header) ->
    false = element(2, Header) == undefined,
    case get_by_hash(hash(Header)) of
        empty -> 
            {ok, PrevHeader} = 
                headers:read(Header#header.prev_hash),
            top(PrevHeader);
        Block -> Block
    end.
height() -> (top())#block.height.
lg(X) when (is_integer(X) and (X > 0)) ->
    lgh(X, 0).
lgh(1, X) -> X;
lgh(N, X) -> lgh(N div 2, X+1).
get_by_height(N) ->
    get_by_height_in_chain(N, headers:top()).
get_by_height_in_chain(N, BH) when N > -1 ->
    Block = get_by_hash(hash(BH)),
    case Block of
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
                    PrevHash = prev_hash(lg(D), Block),
                    {ok, PrevHeader} = headers:read(PrevHash),
                    get_by_height_in_chain(N, PrevHeader)
            end
    end.
prev_hash(0, BP) -> BP#block.prev_hash;
prev_hash(N, BP) -> element(N+1, BP#block.prev_hashes).
time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis_maker() ->
    Root0 = constants:root0(),
    Pub = constants:master_pub(),
    First = accounts:new(Pub, constants:initial_coins()),
    Accounts0 = accounts:write(First, Root0),
    GovInit = governance:genesis_state(),
    Trees0 = trees:new(Accounts0, Root0, Root0, Root0, Root0, GovInit),
    Accounts = accounts:write(First, Root0),
    Trees = trees:new(Accounts, Root0, Root0, Root0, Root0, GovInit),
    TreesRoot = trees:root_hash(Trees),
    BlockPeriod = governance:get_value(block_period, GovInit),
    #block{height = 0,
           prev_hash = <<0:(constants:hash_size()*8)>>,
           txs = [],
           trees_hash = TreesRoot,
           time = 0,
           difficulty = constants:initial_difficulty(),
           period = BlockPeriod,
           version = version:doit(0),
           trees = Trees,
           roots = make_roots(Trees)
          }.
block_reward_dict(Dict, Height, ID, PH) ->
    BCM = 100,
    BlocksAgo = Height - BCM,
    case BlocksAgo > 0 of
        true ->
            Txs = (get_by_height_in_chain(BlocksAgo, PH))#block.txs,
            TransactionFees = txs:fees(Txs),
            TransactionCosts = tx_costs_dict(Txs, Dict, 0),
            BlockReward = dict:fetch({governance, 
                                      governance:name2number(block_reward)},
                                     Dict),
            Amount = BlockReward + TransactionFees - TransactionCosts,
            NM = case accounts:dict_get(ID, Dict) of
                     empty ->  accounts:new(ID, Amount);
                     _ -> accounts:dict_update(ID, Dict, Amount, none)
                 end,
            accounts:dict_write(NM, Dict);
        false -> Dict
    end.
   
tx_costs_dict([], _, Out) -> Out;
tx_costs_dict([STx|T], Dict, Out) ->
    Tx = testnet_sign:data(STx),
    Type = element(1, Tx),
    Cost = dict:fetch({governance, governance:name2number(Type)},
                      Dict),
    tx_costs_dict(T, Dict, Cost+Out).
tx_costs([], _, Out) -> Out;
tx_costs([STx|T], Governance, Out) ->
    Tx = testnet_sign:data(STx),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    tx_costs(T, Governance, Cost+Out).
new_dict(Txs, Dict, Height, _Pub, _PrevHash) ->
    Dict2 = txs:digest_from_dict(Txs, Dict, Height),
    Dict2.
    
make(Header, Txs0, Trees, Pub) ->
    {CB, _Proofs} = coinbase_tx:make(Pub, Trees),
    Txs = [CB|lists:reverse(Txs0)],
    Querys = proofs:txs_to_querys(Txs, Trees),
    Height = Header#header.height,
    Facts = proofs:prove(Querys, Trees),
    Dict = proofs:facts_to_dict(Facts, dict:new()),
    NewDict = new_dict(Txs, Dict, Height+1, keys:pubkey(), hash(Header)),
    NewTrees = dict_update_trie(Trees, NewDict),
    Governance = trees:governance(NewTrees),
    BlockPeriod = governance:get_value(block_period, Governance),
    Block = #block{height = Height + 1,
		   prev_hash = hash(Header),
		   txs = Txs,
		   trees_hash = trees:root_hash(NewTrees),
		   time = time_now(),
		   difficulty = headers:difficulty_should_be(Header),
                   period = BlockPeriod,
		   version = version:doit(Height+1),%constants:version(),
		   trees = NewTrees,
		   prev_hashes = calculate_prev_hashes(Header),
		   proofs = Facts,
                   roots = make_roots(Trees)
		  },
    Block = packer:unpack(packer:pack(Block)),
    %_Dict = proofs:facts_to_dict(Proofs, dict:new()),
    Block.
make_roots(Trees) ->
    #roots{accounts = trie:root_hash(accounts, trees:accounts(Trees)),
           channels = trie:root_hash(channels, trees:channels(Trees)),
           existence = trie:root_hash(existence, trees:existence(Trees)),
           oracles = trie:root_hash(oracles, trees:oracles(Trees)),
           governance = trie:root_hash(governance, trees:governance(Trees))}.
roots_hash(X) when is_record(X, roots) ->
    A = X#roots.accounts,
    C = X#roots.channels,
    E = X#roots.existence,
    O = X#roots.oracles,
    G = X#roots.governance,
    hash:doit(<<A/binary, C/binary, E/binary, 
                         O/binary, G/binary>>).
    
guess_number_of_cpu_cores() ->
    case application:get_env(ae_core, test_mode) of
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
            {ok, CoresToMine} = application:get_env(ae_core, cores_to_mine),
            min(Y, CoresToMine)
    end.
spawn_many(0, _) -> ok;
spawn_many(N, F) -> 
    spawn(F),
    spawn_many(N-1, F).
mine(Rounds) -> 
    %PB = top(),
    %Top = block_to_header(PB),
    TP = tx_pool:get(),
    Txs = TP#tx_pool.txs,
    T = TP#tx_pool.height,
    PB = get_by_height(T),
    Top = block_to_header(PB),
    Block = make(Top, Txs, PB#block.trees, keys:pubkey()),
    mine(Block, Rounds).
mine(Block, Rounds) ->
    %Cores = guess_number_of_cpu_cores(),
    Cores = 1, %slow down mining so I don't break the computer.
    mine(Block, Rounds, Cores).
mine(Block, Rounds, Cores) ->
    F = fun() ->
                case mine2(Block, Rounds) of
                    false -> false;
                    PBlock ->
                        io:fwrite("found a block"),
                        Header = block_to_header(PBlock),
                        headers:absorb([Header]),
                        block_absorber:save(PBlock),
                        sync:start()
                end
        end,
    spawn_many(Cores-1, F),
    F().
mine2(Block, Times) ->
    PH = Block#block.prev_hash,
    ParentPlus = get_by_hash(PH),
    Trees = ParentPlus#block.trees,
    MineDiff = Block#block.difficulty,
    case pow:pow(hash(Block), MineDiff, Times, constants:hash_size()) of
        false -> false;
        Pow -> Block#block{nonce = pow:nonce(Pow)}
    end.
proofs_roots_match([], _) -> true;
proofs_roots_match([P|T], R) ->
    Tree = proofs:tree(P),
    Root = proofs:root(P),
    Root = 
        case Tree of
            oracle_bets -> Root;
            orders -> Root;
            accounts -> R#roots.accounts;
            channels -> R#roots.channels;
            existence -> R#roots.existence;
            oracles -> R#roots.oracles;
            governance -> R#roots.governance
           end,
    proofs_roots_match(T, R).
            
check(Block) ->
    {ok, LN} = application:get_env(ae_core, light_node),
    Facts = Block#block.proofs,
    Header = block_to_header(Block),
    BlockHash = hash(Block),
    {ok, Header} = headers:read(BlockHash),
    OldBlock = get_by_hash(Block#block.prev_hash),
    OldTrees = OldBlock#block.trees,
    Roots = Block#block.roots,
    PrevStateHash = roots_hash(Roots),
    {ok, PrevHeader} = headers:read(Block#block.prev_hash),
    PrevStateHash = PrevHeader#header.trees_hash,
    PrevStateHash = trees:root_hash2(OldTrees, Roots),
    Txs = Block#block.txs,
    BlockSize = size(packer:pack(Txs)),
    Governance = trees:governance(OldTrees),
    MaxBlockSize = governance:get_value(max_block_size, Governance),
    ok = case BlockSize > MaxBlockSize of
	     true -> 
		 io:fwrite("error, this block is too big\n"),
		 bad;
	     false -> ok
    end,
    case LN of
        true -> 
            %OldSparseTrees = 
            %    facts_to_trie(
            %      Facts, trees:new(empty, empty, empty,
            %                       empty, empty, empty)),
            %PrevTreesHash = trees:root_hash2(OldSparseTrees, Roots),
            %NewTrees2 = dict_update_trie(OldSparseTrees, NewDict),
            %TreesHash = trees:root_hash2(NewTrees2, Roots),
            ok;
        false ->
            ok
    end,
    true = proofs_roots_match(Facts, Roots),
    Dict = proofs:facts_to_dict(Facts, dict:new()),
    Height = Block#block.height,
    PrevHash = Block#block.prev_hash,
    Txs = Block#block.txs,
    Pub = coinbase_tx:from(hd(Block#block.txs)),
    true = no_coinbase(tl(Block#block.txs)),
    NewDict = new_dict(Txs, Dict, Height, Pub, PrevHash),
    %NewTrees = new_trees(Txs, OldTrees, Height, Pub, PrevHash),
    NewTrees3 = dict_update_trie(OldTrees, NewDict),
    Block2 = Block#block{trees = NewTrees3},
    TreesHash = trees:root_hash(Block2#block.trees),
    TreesHash = trees:root_hash2(Block2#block.trees, Roots),
    TreesHash = Header#header.trees_hash,
    TreesHash = Block2#block.trees_hash,
    true = hash(Block) == hash(Block2),
    TreesHash = trees:root_hash2(NewTrees3, Roots),
    {true, Block2}.

    %Initially some things in trees is the atom 'empty'.
    %Once we insert the root stem into the trie, then we instead store a pointer to the root stem. 
dict_update_trie(Trees, Dict) ->
    %do the orders and oracle_bets last, then insert their state roots into the accounts and oracles.
    %pointers are integers, root hashes are binary.
    Keys = dict:fetch_keys(Dict),
    {Orders, Keys2} = get_things(orders, Keys),
    {OracleBets, Keys3} = get_things(oracle_bets, Keys2),
    {Accounts, Keys4} = get_things(accounts, Keys3),
    {Oracles, Keys5} = get_things(oracles, Keys4),
    Dict2 = dict_update_trie_orders(Trees, Orders, Dict),
    Dict3 = dict_update_trie_oracle_bets(Trees, OracleBets,Dict2),
    Trees2 = dict_update_trie_account(Trees, Accounts, Dict3),
    Trees3 = dict_update_trie_oracles(Trees2, Oracles, Dict3),
    dict_update_trie2(Trees3, Keys5, Dict3).
dict_update_trie2(T, [], _) -> T;
dict_update_trie2(Trees, [H|T], Dict) ->
    {Type, Key} = H,
    New = Type:dict_get(Key, Dict),
    Tree = trees:Type(Trees),
    Tree2 = case New of
                empty -> Type:delete(Key, Tree);
                _ -> Type:write(New, Tree)
            end,
    Update = list_to_atom("update_" ++ atom_to_list(Type)),
    Trees2 = trees:Update(Trees, Tree2),
    dict_update_trie2(Trees2, T, Dict).
dict_update_trie_oracles(T, [], _) -> T;
dict_update_trie_oracles(Trees, [H|T], Dict) ->
    Trees2 = dict_update_account_oracle_helper(oracles, H, orders, Trees, orders:empty_book(), set_orders, Dict),
    dict_update_trie_oracles(Trees2, T, Dict).
dict_update_trie_account(T, [], _) -> T;
dict_update_trie_account(Trees, [H|T], Dict) ->
    Trees2 = dict_update_account_oracle_helper(accounts, H, bets, Trees, constants:root0(), update_bets, Dict),
    dict_update_trie_account(Trees2, T, Dict).

dict_update_account_oracle_helper(Type, H, Type2, Trees, EmptyType2, UpdateType2, Dict) ->
    {_, Key} = H,
    New0 = Type:dict_get(Key, Dict),
    Tree = trees:Type(Trees),
    Tree2 = 
        case New0 of
            empty -> 
                Type:delete(Key, Tree);
            _ -> 
                ABN = Type:Type2(New0),
                {_, Old, _} = Type:get(Key, trees:Type(Trees)),
                New = if
                          Old == empty -> 
                              Type:UpdateType2(New0, EmptyType2);
                          true ->
                              ABO = Type:Type2(Old),
                              if
                                  ABO == 0 -> 
                                      throw("dict update trie account oracle"),
                                      New0;
                                  0 == ABN -> 
                                      Type:UpdateType2(New0, Type:Type2(Old));
                                  true -> New0
                              end
                      end,
                Type:write(New, Tree)
    end,
    Update = list_to_atom("update_" ++ atom_to_list(Type)),
    trees:Update(Trees, Tree2).
dict_update_trie_orders(_, [], D) -> D;
dict_update_trie_orders(Trees, [H|T], Dict) ->
    {orders, Key} = H,
    {key, Pub, OID} = Key,
    PS = constants:pubkey_size()*8,
    case Pub of
        <<0:PS>> -> throw(dict_update_trie_orders_error);
        _ -> ok
    end,
    {_, Oracle, _} = oracles:get(OID, trees:oracles(Trees)),
    DictOracle = oracles:dict_get(OID, Dict),
    Orders = case DictOracle#oracle.orders of
                 0 -> Oracle#oracle.orders;
                 Z -> Z
             end,
    Orders3 = 
        case Pub of
            <<1:PS>> ->
                %update the header.
                S = dict:fetch(H, Dict),
                {Pointer, Many} = orders:deserialize_head(S),
                orders:head_put(Pointer, Many, Orders);
            _ ->
                New = orders:dict_get(Key, Dict),
                Orders2 = 
                    case New of
                        empty -> orders:delete(Pub, Orders);
                        _ -> orders:write(New, Orders)
                    end,
                Orders2
        end,
    Dict2 = oracles:dict_write(DictOracle, Orders3, Dict),
    dict_update_trie_orders(Trees, T, Dict2).
dict_update_trie_oracle_bets(_, [], D) -> D;
dict_update_trie_oracle_bets(Trees, [H|T], Dict) ->
    {oracle_bets, Key} = H,
    {key, Pub, OID} = Key,
    New = oracle_bets:dict_get(Key, Dict),
    DictAccount = accounts:dict_get(Pub, Dict),
    {_, Account, _} = accounts:get(Pub, trees:accounts(Trees)),
    OracleBets = 
        case DictAccount#acc.bets of
            0 -> Account#acc.bets;
            Z -> Z
        end,
    OracleBets2 = 
        case New of
            empty -> oracle_bets:delete(OID, OracleBets);
            _ -> oracle_bets:write(New, OracleBets)
        end,
    Dict2 = accounts:dict_write(DictAccount, OracleBets2, Dict),
    dict_update_trie_oracle_bets(Trees, T, Dict2).
get_things(Key, L) ->
    get_things(Key, L, [], []).
get_things(Key, [], A, B) -> {A, B};
get_things(Key, [{Key, X}|L], A, B) ->
    get_things(Key, L, [{Key, X}|A], B);
get_things(Key, [{Key2, X}|L], A, B) ->
    get_things(Key, L, A, [{Key2, X}|B]).
facts_to_trie([], Tree) -> Tree;
facts_to_trie([Fact|T], Tree) ->
    Tree2 = ftt2(Fact, Tree),
    facts_to_trie(T, Tree2).
setup_tree(Empty, Start, Path, Type) ->
    case Start of
        Empty ->
            Hashes = hd(lists:reverse(Path)),
            Stem = stem:make(Hashes, Type),
            trie:new_trie(Type, Stem);
        X -> X
    end.
ftt2(Fact, Trees) ->
    Type = proofs:tree(Fact),
    case Type of
        orders ->
            {key, _Pubkey, OID} = proofs:key(Fact),
            Oracles = trees:oracles(Trees),
            Path = proofs:path(Fact),
            {_, Oracle, _} = oracles:get(OID, Oracles),
            case Oracle of 
                empty -> 
                    Trees;
                _ -> 
                    Orders = Oracle#oracle.orders,
                    Orders2 = setup_tree(0, Orders, Path, Type),
                    Orders3 = trees:restore(Orders2, Fact, 0),
                    Oracle2 = oracles:set_orders(Oracle, Orders3),
                    Oracles2 = oracles:write(Oracle2, Oracles),
                    trees:update_oracles(Trees, Oracles2)
            end;
        oracle_bets -> 
            {key, Pubkey, _OID} = proofs:key(Fact),
            Path = proofs:path(Fact),
            Accounts = trees:accounts(Trees),
            {_, Account, _} = accounts:get(Pubkey, Accounts),
            Bets = Account#acc.bets,
            Bets2 = setup_tree(0, Bets, Path, Type),
            Bets3 = trees:restore(Bets2, Fact, 0),
            Account2 = accounts:update_bets(Account, Bets3),
            Accounts2 = accounts:write(Account2, Accounts),
            trees:update_accounts(Trees, Accounts2);
        _ ->
            Path = proofs:path(Fact),
            Tree = setup_tree(empty, trees:Type(Trees), Path, Type),
            Tree2 = trees:restore(Tree, Fact, 0),
            Update = list_to_atom("update_" ++ atom_to_list(Type)),
            trees:Update(Trees, Tree2)
    end.
no_coinbase([]) -> true;
no_coinbase([STx|T]) ->
    Tx = testnet_sign:data(STx),
    Type = element(1, Tx),
    false = Type == coinbase,
    no_coinbase(T).

initialize_chain() -> 
    %only run genesis maker once, or else it corrupts the database.
    {ok, L} = file:list_dir("blocks"),
    B = length(L) < 1,
    GB = if
        B -> G = genesis_maker(),
             block_absorber:do_save(G),
             G;
        true -> get_by_height(0)
         end,
    Header0 = block_to_header(GB),
    Header0.

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
    headers:absorb([Header1]),
    H1 = hash(Header1),
    H1 = hash(WBlock10),
    {ok, _} = headers:read(H1),
    block_absorber:save(WBlock10),
    WBlock11 = get_by_hash(H1),
    WBlock11 = get_by_height_in_chain(1, H1),
    WBlock10 = WBlock11#block{trees = WBlock10#block.trees},
    success;
test(2) ->
    {_, _, Proofs} = accounts:get(keys:pubkey(), 1),
    _Proof = hd(Proofs).

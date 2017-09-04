-module(block).

-export([block_to_header/1, test/0,
         height/1, prev_hash/1, txs/1, trees_hash/1, time/1, difficulty/1, comment/1, version/1, pow/1, trees/1, prev_hashes/1, 
         get_by_height_in_chain/2, get_by_height/1, hash/1, get_by_hash/1, initialize_chain/0, make/4,
         mine/1, mine/2, mine2/2, check/1, 
         guess_number_of_cpu_cores/0, top/0
        ]).

-export_type([block/0]).

-record(block, {height :: headers:height(),
                prev_hash :: headers:block_header_hash(),
                trees_hash,
                time,
                difficulty,
                version,
                nonce = 0,
                trees,
                txs,
                prev_hashes = {prev_hashes},
                proofs = [],
                comment = <<>>,
                roots}).
-record(roots, {accounts, channels, existence, burn, oracles, governance}).

%proofs is for this
%If the attacker sends a valid block with a valid header,
% but makes one of the proofs of the state tree wrong.
%The node getting attacked would have to verify all the proofs until it found a bad one.
%I think this might be making it too affordable to DDOS our nodes.
%So I am thinking of adding something more to the header.
%A hash of a datastructure containing all the proofs for this block.
%So when you are loading the block,
%you can know if a proof has been manipulated immediately.

-opaque block() :: #block{}.

height(B) -> B#block.height.
prev_hash(B) -> B#block.prev_hash.
txs(B) -> B#block.txs.
trees_hash(B) -> B#block.trees_hash.
time(B) -> B#block.time.
difficulty(B) -> B#block.difficulty.
comment(B) -> B#block.comment.
version(B) -> B#block.version.
pow(B) -> B#block.nonce.
trees(B) -> B#block.trees.
prev_hashes(B) -> B#block.prev_hashes.
proofs(B) -> B#block.proofs.

txs_proofs_hash(Txs, Proofs) ->
    testnet_hasher:doit({Txs, Proofs}).
block_to_header(B) ->
    headers:make_header(B#block.prev_hash,
                        B#block.height,
                        B#block.time,
                        B#block.version,
                        B#block.trees_hash,
                        txs_proofs_hash(B#block.txs, B#block.proofs),
                        B#block.nonce,
                        B#block.difficulty).

-spec hash(block() |
           headers:header() |
           headers:serialized_header() |
           headers:block_header_hash()
          ) -> headers:block_header_hash();
          (error) -> error.
hash(error) -> error;
hash(B) when is_binary(B) ->
    case size(B) == constants:hash_size() of
        true ->
            B;
        false ->
            testnet_hasher:doit(B)
    end;
hash(B) when element(1, B) ==  header ->
    hash(headers:serialize(B));
hash(B) when is_record(B, block) ->
    hash(block_to_header(B)).

calculate_prev_hashes(Parent) ->
    H = headers:height(Parent),
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

-spec get_by_hash(Hash) -> empty | block() when
      Hash :: block()
            | headers:header()
            | headers:serialized_header()
            | headers:block_header_hash().
get_by_hash(H) ->
    Hash = hash(H),
    BlockFile = ae_utils:binary_to_file_path(blocks, Hash),
    case db:read(BlockFile) of
        [] -> empty;
        Block -> binary_to_term(zlib:uncompress(Block))
    end.

-spec top() -> block().
top() ->
    TH = headers:top(),
    top(TH).

top(Header) ->
    case get_by_hash(hash(Header)) of
        empty -> 
            {ok, PrevHeader} = headers:read(headers:prev_hash(Header)),
            top(PrevHeader);
        Block -> Block
    end.

lg(X) when is_integer(X) andalso X > 0 ->
    lgh(X, 0).
lgh(1, X) ->
    X;
lgh(N, X) ->
    lgh(N div 2, X+1).

-spec get_by_height(headers:height()) -> empty | block().
get_by_height(N) ->
    get_by_height_in_chain(N, headers:top()).

-spec get_by_height_in_chain(headers:height(), Chain::headers:header()) ->
                                    empty | block().
get_by_height_in_chain(N, BH) when N > -1 ->
    Block = get_by_hash(hash(BH)),
    case Block of
        empty ->
            PrevHash = headers:prev_hash(BH),
            {ok, PrevHeader} = headers:read(PrevHash),
            get_by_height_in_chain(N, PrevHeader);
        _  ->
            M = height(Block),
            D = M - N,
            if
                D < 0 ->
                    empty;
                D == 0 -> Block;
                true ->
                    PrevHash = prev_hash(lg(D), Block),
                    {ok, PrevHeader} = headers:read(PrevHash),
                    get_by_height_in_chain(N, PrevHeader)
            end
    end.


prev_hash(0, BP) ->
    prev_hash(BP);
prev_hash(N, BP) -> %N=0 should be the same as prev_hash(BP)
    element(N+1, BP#block.prev_hashes).

time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis_maker() ->
    Root0 = constants:root0(),
    Pub = constants:master_pub(),
    First = accounts:new(Pub, constants:initial_coins(), 0),
    Accounts0 = accounts:write(Root0, First),
    GovInit = governance:genesis_state(),
    Trees0 = trees:new(Accounts0, Root0, Root0, Root0, Root0, GovInit),
    Accounts = accounts:write(Root0, First),
    Trees = trees:new(Accounts, Root0, Root0, Root0, Root0, GovInit),

    TreesRoot = trees:root_hash(Trees),
    #block{height = 0,
           prev_hash = <<0:(constants:hash_size()*8)>>,
           txs = [],
           trees_hash = TreesRoot,
           time = 0,
           difficulty = constants:initial_difficulty(),
           version = constants:version(),
           trees = Trees,
           roots = make_roots(Trees)
          }.
block_reward(Trees, Height, ID, PH) -> 
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    %BCM = governance:get_value(block_creation_maturity, Governance),
    BCM = 100,
    BlocksAgo = Height - BCM,
    case BlocksAgo > 0 of
        true ->
            Txs = txs(get_by_height_in_chain(BlocksAgo, PH)),
            TransactionFees = txs:fees(Txs),
            TransactionCosts = tx_costs(Txs, Governance, 0),
            BlockReward = governance:get_value(block_reward, Governance),
            Amount = BlockReward + TransactionFees - TransactionCosts,
            NM = case accounts:get(ID, OldAccounts) of
                     {_, empty,_} ->  accounts:new(ID, Amount, Height);
                     _ -> accounts:update(ID, Trees, Amount, none, Height)
                 end,
            NewAccounts = accounts:write(OldAccounts, NM),
            trees:update_accounts(Trees, NewAccounts);
        false ->
            Trees
    end.
block_reward_dict(Dict, Height, ID, PH) ->
    BCM = 100,
    BlocksAgo = Height - BCM,
    case BlocksAgo > 0 of
        true ->
            Txs = txs(get_by_height_in_chain(BlocksAgo, PH)),
            TransactionFees = txs:fees(Txs),
            TransactionCosts = tx_costs_dict(Txs, Dict, 0),
            BlockReward = dict:fetch({governance, 
                                      governance:name2number(block_reward)},
                                     Dict),
            Amount = BlockReward + TransactionFees - TransactionCosts,
            NM = case dict:fetch({accounts, ID}, Dict) of
                     empty ->  accounts:new(ID, Amount, Height);
                     _ -> accounts:dict_update(ID, Dict, Amount, none, Height)
                 end,
            dict:store({accounts, ID}, NM, Dict);
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
new_dict(Txs, Dict, Height, Pub, PrevHash) ->
    Dict2 = txs:digest_from_dict(Txs, Dict, Height),
    block_reward_dict(Dict2, Height, Pub, PrevHash).
    
new_trees(Txs, Trees, Height, Pub, HeaderHash) -> 
%convert trees to dictionary format
    Trees2 = txs:digest(Txs, Trees, Height),
    block_reward(Trees2, Height, Pub, HeaderHash).
%convert back to merkle tree format.
make(Header, Txs0, Trees, Pub) ->
    {CB, _Proofs} = coinbase_tx:make(Pub, Trees),
    Txs = [keys:sign(CB)|Txs0],
    Querys = proofs:txs_to_querys(Txs, Trees),
    Height = headers:height(Header),
    NewTrees = new_trees(Txs, Trees, Height+1, Pub, hash(Header)),
    Proofs = proofs:prove(Querys, Trees),
    Block = #block{height = Height + 1,
		   prev_hash = hash(Header),
		   txs = Txs,
		   trees_hash = trees:root_hash(NewTrees),
		   time = time_now(),
		   difficulty = headers:difficulty_should_be(Header),
		   version = constants:version(),
		   trees = NewTrees,
		   prev_hashes = calculate_prev_hashes(Header),
		   proofs = Proofs,
                   roots = make_roots(Trees)
		  },
    Block = packer:unpack(packer:pack(Block)),
    _Dict = proofs:facts_to_dict(Proofs, dict:new()),
    Block.
make_roots(Trees) ->
    #roots{accounts = accounts:root_hash(trees:accounts(Trees)),
           channels = channels:root_hash(trees:channels(Trees)),
           existence = existence:root_hash(trees:existence(Trees)),
           burn = burn:root_hash(trees:burn(Trees)),
           oracles = oracles:root_hash(trees:oracles(Trees)),
           governance = governance:root_hash(trees:governance(Trees))}.
roots_hash(X) when is_record(X, roots) ->
    A = X#roots.accounts,
    C = X#roots.channels,
    E = X#roots.existence,
    B = X#roots.burn,
    O = X#roots.oracles,
    G = X#roots.governance,
    testnet_hasher:doit(<<A/binary, C/binary, E/binary, 
                         B/binary, O/binary, G/binary>>).
    
guess_number_of_cpu_cores() ->
    case application:get_env(ae_core, test_mode, false) of
        true -> 1;
        false ->
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
    spawn(F()),
    spawn_many(N-1, F).

mine(Rounds) -> 
    Top = headers:top(),
    %timer:sleep(100),
    PB = block:get_by_hash(Top),
    {_, _, Txs} = tx_pool:data(),
    Block = block:make(Top, Txs, block:trees(PB), keys:pubkey()),
    mine(Block, Rounds).

mine(Block, Rounds) ->
    Cores = guess_number_of_cpu_cores(),
    mine(Block, Rounds, Cores).
mine(Block, Rounds, Cores) ->
    F = fun() ->
                case mine2(Block, Rounds) of
                    false -> false;
                    PBlock ->
                        lager:info("Found a block: ~p", [integer_to_list(height(PBlock))]),
                        Header = block_to_header(PBlock),
                        headers:absorb([Header]),
                        block_absorber:save(PBlock)
                end
        end,
    spawn_many(Cores-1, F),
    F().
mine2(Block, Times) ->
    PH = Block#block.prev_hash,
    ParentPlus = get_by_hash(PH),
    Trees = ParentPlus#block.trees,
    Difficulty = Block#block.difficulty,
    Governance = trees:governance(Trees),
    BlockReward = governance:get_value(block_reward, Governance),
    MineDiff = (Difficulty * BlockReward) div constants:initial_block_reward(),
    case pow:pow(hash(Block), MineDiff, Times, constants:hash_size()) of
        false -> false;
        Pow ->
            Nonce = pow:nonce(Pow),
            B2 = Block#block{nonce = Nonce},
            B2
    end.
proofs_roots_match([], _) -> true;
proofs_roots_match([P|T], R) ->
    Tree = proofs:tree(P),
    Root = proofs:root(P),
    case Tree of
        accounts ->
            true = R#roots.accounts == Root;
        channels -> 
            true = R#roots.channels == Root;
        existence -> 
            true = R#roots.existence == Root;
        burn -> 
            true = R#roots.burn == Root;
        oracles ->
            true = R#roots.oracles == Root;
        governance ->
            true = R#roots.governance == Root;
        _ -> ok
    end,
    proofs_roots_match(T, R).
            
check(Block) ->
    Facts = Block#block.proofs,
    Header = block_to_header(Block),
    BlockHash = hash(Block),
    {ok, Header} = headers:read(BlockHash),

    OldBlock = get_by_hash(Block#block.prev_hash),
    OldTrees = OldBlock#block.trees,
    PrevStateHash = roots_hash(Block#block.roots),
    {ok, PrevHeader} = headers:read(Block#block.prev_hash),
    PrevStateHash = headers:trees_hash(PrevHeader),
    Roots = Block#block.roots,
    true = proofs_roots_match(Block#block.proofs, Roots),
    %GovQueries = proofs:governance_to_querys(trees:governance(OldTrees)),
    %GovProofs = proofs:prove(GovQueries, OldTrees),
    %Dict = proofs:facts_to_dict(Block#block.proofs ++ GovProofs, dict:new()),
    Dict = proofs:facts_to_dict(Facts, dict:new()),
    %load the data into a dictionary, feed this dictionary into new_trees/ instead of OldTrees.
    Height = Block#block.height,
    PrevHash = Block#block.prev_hash,
    Txs = Block#block.txs,
    Pub = coinbase_tx:from(testnet_sign:data(hd(Block#block.txs))),
    true = no_coinbase(tl(Block#block.txs)),
    NewDict = new_dict(Txs, Dict, Height, Pub, PrevHash),
    %OldSparseTree = facts_to_trie(Facts, trees:new(empty, empty, empty,
    %                                               empty, empty, empty)),
    %NewTrees = dict_update_trie(Roots, 
    %                            OldSparseTree,
    %                            NewDict),
    %use NewDict to generate NewTrees
    NewTrees = new_trees(Txs, OldTrees, Height, Pub, PrevHash),
    Block2 = Block#block{trees = NewTrees},
    TreesHash = trees:root_hash(Block2#block.trees),
    TreesHash = headers:trees_hash(Header),
    TreesHash = Block2#block.trees_hash,
    true = hash(Block) == hash(Block2),
    {true, Block2}.

    %Initially some things in trees is the atom 'empty'.
    %Once we insert the root stem into the trie, then we instead store a pointer to the root stem. 
dict_update_trie(Roots, Trees, Dict) ->
    %do the orders and oracle_bets first, then insert their state roots into the accounts and oracles.
    %pointers are integers, root hashes are binary.
    Keys = dict:fetch_keys(Dict),
    {Orders, Keys2} = get_things(orders, Keys),
    {OracleBets, Keys3} = get_things(oracle_bets, Keys2),
    %{Accounts, Keys4} = get_things(accounts, Keys3),
    %{Oracles, Keys5} = get_things(oracles, Keys4),
    
    Dict.
get_things(Key, L) ->
    get_things(Key, L, [], []).
get_things(Key, [], A, B) -> {A, B};
get_things(Key, [{Key, X}|L], A, B) ->
    get_things(Key, L, [{Key, X}|A], B);
get_things(Key, [{Key2, X}|L], A, B) ->
    get_things(Key, L, A, [{Key, X}|B]).
facts_to_trie([], Tree) -> Tree;
facts_to_trie([Fact|T], Tree) ->
    %io:fwrite("facts to trie trees are "),
    %io:fwrite(packer:pack(Tree)),
    %io:fwrite("\n"),
    %facts are proofs.
    Tree2 = ftt2(Fact, Tree),
    facts_to_trie(T, Tree2).
ftt2(Fact, Trees) ->
    Type = proofs:tree(Fact),
    io:fwrite("tree type is "),
    io:fwrite(Type),
    io:fwrite("\n"),
    %Tree = trees:Type(Trees),
    Path = proofs:path(Fact),
    Tree = case trees:Type(Trees) of
               empty -> %make pointer to empty
                   Hashes = hd(lists:reverse(Path)),
                   Stem = stem:make(stem:empty_tuple(), 
                                    stem:empty_tuple(), 
                                    Hashes),
                   %io:fwrite("new trie "),
                   %io:fwrite(packer:pack(Stem)),
                   %io:fwrite("\n"),
                   trie:new_trie(Type, Stem);
               X -> X
           end,
    V = proofs:value(Fact),
    Key = proofs:key(Fact),
    %io:fwrite("fact is "),
    %io:fwrite(packer:pack(Fact)),
    %io:fwrite("\n"),
    %Leaf = Type:make_leaf(Key, Type:serialize(V), trie:cfg(Type)),
    Leaf = Type:make_leaf(Key, V, trie:cfg(Type)),
    %io:fwrite("about to root hash \n"),
    %io:fwrite(packer:pack({Type, Tree})),
    %io:fwrite("\n"),
    Hash = Type:root_hash(Tree),
    io:fwrite("about to store \n"),
    io:fwrite(packer:pack({leaf, Tree, Leaf})),
    io:fwrite(" \n"),
    %io:fwrite(packer:pack({hash_path_tree_type, Hash, Path, Tree, Type})),
    {Hash, Tree2, _} = 
        store:store(Leaf, Hash, 
                    Path, 
                    Tree, 
                    trie:cfg(Type)),
    Update = list_to_atom("update_" ++ atom_to_list(Type)),
    %io:fwrite("update tree "),
    %io:fwrite(Type),
    %io:fwrite(" "),
    %io:fwrite(Update),
    %io:fwrite("trie 2 is "),
    %io:fwrite(packer:pack(Tree2)),
    %io:fwrite("\n"),
    trees:Update(Trees, Tree2).
no_coinbase([]) -> true;
no_coinbase([STx|T]) ->
    Tx = testnet_sign:data(STx),
    Type = element(1, Tx),
    false = Type == coinbase,
    no_coinbase(T).

initialize_chain() -> 
    GB = genesis_maker(),
    block_absorber:do_save(GB),
    Header0 = block_to_header(GB),
    headers:hard_set_top(Header0),
    block_hashes:add(hash(Header0)),
    Header0.


%% Tests

test() ->
    test(1).
test(1) ->
    Header0 = headers:top(),
    Block0 = get_by_hash(Header0),
    Trees = trees(Block0),
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

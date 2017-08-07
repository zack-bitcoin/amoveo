-module(block).

-export([block_to_header/1, test/0,
         height/1, prev_hash/1, txs/1, trees_hash/1, time/1, difficulty/1, comment/1, version/1, pow/1, trees/1, prev_hashes/1, 
         read_int/2, read_int/1, hash/1, read/1, initialize_chain/0, make/4,
         mine/1, mine/2, mine2/2, check/1, 
         guess_number_of_cpu_cores/0, top/0
        ]).

-record(block, {height,
                prev_hash,
                trees_hash,
                time,
                difficulty,
                version,
                nonce = 0,
                trees,
                txs,
                prev_hashes = {prev_hashes},
                proofs = [],
                comment = <<>>}).

%proofs is for this
%If the attacker sends a valid block with a valid header,
% but makes one of the proofs of the state tree wrong.
%The node getting attacked would have to verify all the proofs until it found a bad one.
%I think this might be making it too affordable to DDOS our nodes.
%So I am thinking of adding something more to the header.
%A hash of a datastructure containing all the proofs for this block.
%So when you are loading the block,
%you can know if a proof has been manipulated immediately.
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
            B = read_int(NHeight, PH),
            calculate_prev_hashes([hash(B)|[PH|Hashes]], NHeight, N*2)
    end.

read(H) ->
    Hash = hash(H),
    BlockFile = ae_utils:binary_to_file_path(blocks, Hash),
    case db:read(BlockFile) of
        [] -> empty;
        Block -> binary_to_term(zlib:uncompress(Block))
    end.
top() ->
    TH = headers:top(),
    top(TH).
top(Header) ->
    case read(hash(Header)) of
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

read_int(N) ->
    read_int(N, headers:top()).
read_int(N, BH) when N > -1 ->
    Block = read(hash(BH)),
    case Block of
        empty ->
            PrevHash = headers:prev_hash(BH),
            {ok, PrevHeader} = headers:read(PrevHash),
            read_int(N, PrevHeader);
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
                    read_int(N, PrevHeader),
                    read_int(N, prev_hash(lg(D), Block))
            end
    end.


prev_hash(0, BP) ->
    prev_hash(BP);
prev_hash(N, BP) -> %N=0 should be the same as prev_hash(BP)
    element(N+1, BP#block.prev_hashes).

time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis_maker() ->
    Pub = constants:master_pub(),
    First = accounts:new(Pub, constants:initial_coins(), 0),
    Accounts = accounts:write(0, First),
    GovInit = governance:genesis_state(),
    Trees = trees:new(Accounts, 0, 0, 0, 0, GovInit),
    TreesRoot = trees:root_hash(Trees),
    #block{height = 0,
           prev_hash = <<0:(constants:hash_size()*8)>>,
           txs = [],
           trees_hash = TreesRoot,
           time = 0,
           difficulty = constants:initial_difficulty(),
           version = constants:version(),
           trees = Trees
          }.
block_reward(Trees, Height, ID, PH) -> 
    OldAccounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BCM = governance:get_value(block_creation_maturity, Governance),
    BlocksAgo = Height - BCM,
    case BlocksAgo > 0 of
        true ->
            Txs = txs(read_int(BlocksAgo, PH)),
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
tx_costs([], _, Out) -> Out;
tx_costs([STx|T], Governance, Out) ->
    Tx = testnet_sign:data(STx),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    tx_costs(T, Governance, Cost+Out).

new_trees(Txs, Trees, Height, Pub, HeaderHash) -> 
%convert trees to dictionary format
    Trees2 = txs:digest(Txs, Trees, Height),
    block_reward(Trees2, Height, Pub, HeaderHash).
%convert back to merkle tree format.
make(Header, Txs0, Trees, Pub) ->
    {CB, _Proofs} = coinbase_tx:make(Pub, Trees),
    Txs = [keys:sign(CB)|Txs0],
    Height = headers:height(Header),
    NewTrees = new_trees(Txs, Trees, Height+1, Pub, hash(Header)),
    #block{height = Height + 1,
                   prev_hash = hash(Header),
                   txs = Txs,
                   trees_hash = trees:root_hash(NewTrees),
                   time = time_now(),
                   difficulty = headers:difficulty_should_be(Header),
                   version = constants:version(),
                   trees = NewTrees,
                   prev_hashes = calculate_prev_hashes(Header)
                  }.

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
    PB = block:read(Top),
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
    ParentPlus = read(PH),
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
check(Block) ->
    BlockHash = hash(Block),
    {ok, _} = headers:read(BlockHash),
    OldBlock = read(Block#block.prev_hash),
    OldTrees = OldBlock#block.trees,
    %check that hash(proofs) is the same as the header.
    %check that every proof is valid to the previous state root.
    %load the data into a dictionary, feed this dictionary into new_trees/ instead of OldTrees.
    Height = Block#block.height,
    PrevHash = Block#block.prev_hash,
    Txs = Block#block.txs,
    Pub = coinbase_tx:from(testnet_sign:data(hd(Block#block.txs))),
    NewTrees = new_trees(Txs, OldTrees, Height, Pub, PrevHash),
    Block2 = Block#block{trees = NewTrees},
    TreesHash = trees:root_hash(Block2#block.trees),
    TreesHash = Block2#block.trees_hash,
    true = hash(Block) == hash(Block2),
    {true, Block#block{trees = NewTrees}}.

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
    Block0 = read(Header0),
    Trees = trees(Block0),
    Pub = keys:pubkey(),
    Block1 = make(Header0, [], Trees, Pub),
    WBlock10 = mine2(Block1, 10),
    Header1 = block_to_header(WBlock10),
    headers:absorb([Header1]),
    H1 = hash(Header1),
    H1 = hash(WBlock10),
    {ok, _} = headers:read(H1),
    block_absorber:save(WBlock10),
    WBlock11 = read(H1),
    WBlock11 = read_int(1, H1),
    WBlock10 = WBlock11#block{trees = WBlock10#block.trees},
    success;
test(2) ->
    {_, _, Proofs} = accounts:get(keys:pubkey(), 1),
    _Proof = hd(Proofs).

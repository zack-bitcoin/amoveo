-module(block_tree).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, long_test/0,test/0,write/1,top/0,read/1,read_int/2,read_int/1,secret/4,account/1,account/2,account/3,channel/2,channel/3,channel/1,absorb/1,is_key/1,height/1,height/0,txs/1,txs/0,power/0,power/1,block/0,block/1,buy_block/2, block_power/1,block_entropy/1,empty_block/0,total_coins/0,total_coins/1, buy_block/0, block_number/1, block2txs/1, block_root/1,backup/1,x_to_block/1,check/0,reset/0,creation_cost/1]).
-record(block, {acc = 0, number = 0, hash = "", txs = [], power = fractions:multiply_int(constants:initial_portion_delegated(), constants:initial_coins()), entropy = 0, total_coins = constants:initial_coins(), db_root = <<>>}).
%power is how many coin are in channels. it is for consensus.
%total coins is a little high. It doesn't include the block creation fee from creating the current block
block_root(B) -> B#block.db_root.
block_number(B) -> B#block.number.
block_power(B) -> B#block.power.
block_entropy(B) -> B#block.entropy.
empty_block() -> #block{}.
-record(x, {block = 0, height = 0, parent = finality, accounts = dict:new(), channels = dict:new(), secrets = dict:new()}).%height always increases by 1. 
x_to_block(X) -> X#x.block.
init(ok) -> 
    SignedBlock = block_finality:top_block(),
    Block = testnet_sign:data(SignedBlock),
    N = Block#block.number,
    X = #x{block = SignedBlock, height = N},
    BH = hash:doit(testnet_sign:data(SignedBlock)),
    D = dict:store(top, BH, dict:new()),
    E = dict:store(BH, X, D),
    {ok, E}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.
read_int_internal(Height, BlockPointer, D) ->
    case dict:find(BlockPointer, D) of
	error -> error;
	{ok, BlockX} ->
	    F = constants:finality(),
	    if
						%BlockX == finality -> block_finality:read(Height);
		BlockX#x.height - Height > F -> block_finality:read(Height);
		BlockX#x.height == Height -> BlockX#x.block;
		BlockX#x.parent == finality -> block_finality:read(Height);
		true -> read_int_internal(Height, BlockX#x.parent, D)
	    end
    end.
handle_cast(reset, _) -> 
    {ok, DB} = init(ok),
    {noreply, DB};
handle_cast(_, X) -> {noreply, X}.
handle_call({read_int, Height, ParentKey}, _From, D) -> 
    {reply, read_int_internal(Height, ParentKey, D), D};
handle_call(top, _From, D) -> %this is bad, just use read.
    {reply, dict:fetch(dict:fetch(top, D), D), D};
handle_call({key, X}, _From, D) -> {reply, dict:is_key(X, D), D};
handle_call({read, V}, _From, D) -> 
    X = case dict:find(V, D) of
	    {ok, Value} -> Value;
	    error -> <<"none">>
	end,
    {reply, X, D};
%handle_call({unsafe_write, K, V}, _From, D) -> 
%    NewBlock = testnet_sign:data(V#x.block),
%    ND = dict:store(top, hash:doit(NewBlock), D),
%    {reply, 0, dict:store(K, V, ND)};
handle_call(check, _From, D) -> {reply, D, D};
handle_call({write, K, V}, _From, D) -> 
    T = dict:fetch(top, D),
    Top = dict:fetch(T, D),
    NewBlock = testnet_sign:data(V#x.block),
    TopHeightB = testnet_sign:data(Top#x.block),
    TopHeight = TopHeightB#block.number,
    NewHeight = NewBlock#block.number,
    Finality = constants:finality(),
    L = length(dict:fetch_keys(D)),
    ND = if
	     NewHeight > TopHeight ->
	    %possible pruning, and merge digests into finality.
		 DD = if 
			  L > Finality -> 
			  %NewHeight > Finality ->
			      {Child, OldKey, X} = merger(T, D),
			      Block = testnet_sign:data(X#x.block),
			      BN = block_tree:block_number(Block),
			      A = X#x.accounts,
			      C = X#x.channels,
			      S = X#x.secrets,
			      Z = backup(NewHeight),
			      if
				  Z ->
				      H = backup:hash(),
				      H = NewBlock#block.db_root,
				      backup:backup(),
				      0 = 0;
				  true -> 0 = 0
			      end,
			      if BN > 0 -> block_finality:append(X#x.block, X#x.height); true -> 0 = 0 end,
			      finality_absorb(S, A, C),
			      P = dict:fetch(Child, D),
			      NewP = #x{block = P#x.block, height = P#x.height, parent = finality, accounts = P#x.accounts, channels = P#x.channels, secrets = P#x.secrets},
			      dict:store(Child, NewP, dict:erase(OldKey, D));
			  true  -> D
		      end,
                 txs:dump(),
                 dict:store(top, hash:doit(NewBlock), DD);
	     true -> D
    end,
    {reply, 0, dict:store(K, V, ND)}.
absorb_accounts([], _) -> ok;
absorb_accounts([K|Keys], Accounts) -> 
    accounts:write(K, dict:fetch(K, Accounts)),
    absorb_accounts(Keys, Accounts).
absorb_channels([], _) ->ok;
absorb_channels([Ch|Chs], Channels) ->
    channels:write(Ch, dict:fetch(Ch, Channels)),
    absorb_channels(Chs, Channels).
absorb_secrets([], _) -> ok;
absorb_secrets([K|Keys], Secrets) -> 
    B = dict:fetch(K, Secrets),
    {Height, SH} = K,
    if
	B -> all_secrets:add(Height, SH);
	true -> all_secrets:remove(Height, SH)
    end,
    absorb_secrets(Keys, Secrets).
finality_absorb(Secrets, Accounts, Channels) ->
    AK = dict:fetch_keys(Accounts),
    absorb_accounts(AK, Accounts),
    CK = dict:fetch_keys(Channels),
    absorb_channels(CK, Channels),
    SK = dict:fetch_keys(Secrets),
    absorb_secrets(SK, Secrets).

merger(Key, D) ->
    X = dict:fetch(Key, D),
    case X#x.parent of
	finality -> {<<"none">>, Key, X};
	Y -> merger(Key, Y, D)
    end.
merger(Child, Key, D) ->
    X = dict:fetch(Key, D),
    case X#x.parent of
	finality -> {Child, Key, X};
	Y -> merger(Key, Y, D)
    end.
reset() -> gen_server:cast(?MODULE, reset).
top() -> gen_server:call(?MODULE, top).
is_key(X) -> gen_server:call(?MODULE, {key, X}).
read(K) -> gen_server:call(?MODULE, {read, K}).
creation_cost(Block) ->
    case Block#block.number of
	0 -> 0;
	N ->
	    X = read(Block#block.hash),
	    PH = (testnet_sign:data(X#x.block))#block.number,
	    BlockGap = N - PH - 1,
	    fractions:multiply_int(constants:block_creation_fee(), Block#block.total_coins * round(math:pow(2, BlockGap)))
    end.
tx_cost(Block) ->
    NewTotalCoins = 
	case Block#block.number of
	    0 -> Block#block.total_coins;
	    N ->
		Parent = (testnet_sign:data((read(Block#block.hash))#x.block)),
		{_, _, NTC, _} = txs:digest(Block#block.txs, Block#block.hash, dict:new(), dict:new(), Parent#block.total_coins, dict:new(), N),
		NTC
	end,
    Block#block.total_coins - NewTotalCoins.
total_coins() -> total_coins(testnet_sign:data(block(read(read(top))))).
total_coins(Block) -> 
    Block#block.total_coins - creation_cost(Block) - tx_cost(Block).
block() -> block(read(read(top))).
block(X) when is_record(X, x) -> 
    X#x.block;
%block(<<"none">>) -> 1=2;
block(X) -> 
    true = size(X) > 4,
    block(read(X)).
%if
	%is_record(X, x) -> testnet_sign:data(X#x.block);
%is_record(X, x) -> X#x.block;
%true -> block(read(X))
%end.
block2txs(X) -> X#block.txs.
txs() -> txs(read(read(top))).
txs(X) -> 
    B = testnet_sign:data(X),
    B#block.txs.
power() -> power(read(read(top))).
power(X) -> 
    A = element(1, X),
    B = case A of
	x -> testnet_sign:data(X#x.block);
	signed -> testnet_sign:data(X);
	block -> X
    end,
    B#block.power.
height() -> height(read(top)).
height(K) -> 
    X = read(K),
    X#x.height.
read_int(Height) -> 
    true = Height > -1,
    true = Height < height() + 1,
    read_int(Height, read(top)).
read_int(Height, BlockPointer) ->
    true = Height > -1,
    gen_server:call(?MODULE, {read_int, Height, BlockPointer}).
write(SignedBlock) ->
    Block = testnet_sign:data(SignedBlock),
    BH = hash:doit(Block),
    write2(is_key(BH), SignedBlock).
write2(true, _) -> ok;
write2(false, SignedBlock) ->
    Block = testnet_sign:data(SignedBlock),
    ParentKey = Block#block.hash,
    Parentx = read(ParentKey),
    Parent = testnet_sign:data(Parentx#x.block),%we need to charge more if this skipped height. 
    OldNumber = Parent#block.number,
    NewNumber = Block#block.number,
    BlockGap = NewNumber - OldNumber,
    true = BlockGap > 0,
%check that it has sign txs that validate it's parent block.
%each sign tx may have won multiple times.
    Winners = sign_tx:winners(Block#block.txs),
    true = Winners > (constants:minimum_validators_per_block() - 1),
%check that the amount bonded is within a small margin of the average of the last several blocks. Check that the amount being spent is less than 1/2 the amount bonded.
    Size = size(zlib:compress(term_to_binary(Block))),
    true = Size < constants:max_block_size(),
    Entropy = entropy:doit(NewNumber),
    Entropy = Block#block.entropy, 
    {ChannelsDict, AccountsDict, NewTotalCoins, Secrets} = txs:digest(Block#block.txs, ParentKey, dict:new(), dict:new(), Parent#block.total_coins, dict:new(), NewNumber),
    %NewTotalCoins = Block#block.total_coins,
%take fee from block creator in the digest.
    %TCIncreases and CCLosses this way is no good.
    %Instead, look at tc increases in the most recent block, and cc_losses in the most recent block. The estimate is less precise, but more accurate. The estimate has a bigger bell curve, but at least the bell curve's center can't be adjusted by an adversary. 
    TcIncreases = to_channel_tx:tc_increases(Block#block.txs),
    CCLosses = channel_block_tx:cc_losses(Block#block.txs),
    RepoLosses = repo_tx:losses(Block#block.txs),
    CFLLosses = channel_funds_limit_tx:losses(Block#block.txs, dict:new(), ParentKey),
    NewPower = power(Parentx#x.block) + TcIncreases - CCLosses - RepoLosses - CFLLosses,%increases from to_channel tx fed into finality (when the channel is still open) - decreases from channel closures in this block (for channels that have been open since finality).
    NewPower = power(SignedBlock),
    NewHeight = Parentx#x.height + BlockGap,
    CreationCost = creation_cost(Block),
    CreatorId = Block#block.acc,
    NTC = NewTotalCoins - CreationCost,
    NewCreator = accounts:update(account(CreatorId), NewHeight, -CreationCost, 0, 0, NTC),
    NewAccountsDict = dict:store(CreatorId, NewCreator, AccountsDict),
    V = #x{accounts = NewAccountsDict, channels = ChannelsDict, block = SignedBlock, parent = ParentKey, height = NewHeight, secrets = Secrets},
    %possibly change top block, and prune one or more blocks, and merge a block with the finality databases.
    %NTC = total_coins(Block),%remove this line for speed, add it back for sanity checking.
    Key = hash:doit(testnet_sign:data(SignedBlock)),
    gen_server:call(?MODULE, {write, Key, V}),
    tx_pool:dump(NTC).
absorb([]) -> ok;
absorb([Block|T]) -> write(Block), absorb(T).
%secret(N, SH) -> secret(N, SH, tx_pool:secrets()).
%secret(N, SH, SecretsDict) -> secret(N, SH, read(top), SecretsDict).
check() ->
    gen_server:call(?MODULE, check).
secret(N, SH, Block, SecretsDict) ->
    Key = {N, SH},
    B = dict:is_key(Key, SecretsDict),
    if
        B -> dict:fetch(Key, SecretsDict);
        true -> secret_helper(Key, Block)
    end.
secret_helper({N, SH}, finality) -> 
    io:fwrite("Check finality"),
    all_secrets:exists(N, SH);
secret_helper(Key, Block) -> 
    X = read(Block),
    Secrets = X#x.secrets,
    Parent = X#x.parent,
    case dict:find(Key, Secrets) of
	error -> secret_helper(Key, Parent);
	{ok, Val} -> Val
    end.

account(N) -> account(N, tx_pool:accounts()).
account(N, AccountsDict) -> account(N, read(top), AccountsDict).
account(N, H, AccountsDict) ->
    B = dict:is_key(N, AccountsDict),
    if
        B -> dict:fetch(N, AccountsDict);
        true -> account_helper(N, H)
    end.
account_helper(N, finality) -> accounts:read_account(N);
account_helper(N, H) ->
    X = read(H),
    Accounts = X#x.accounts,
    Parent = X#x.parent,
    case dict:find(N, Accounts) of
        error -> account_helper(N, Parent);
        {ok, Val} -> Val
    end.
channel(N) -> channel(N, tx_pool:channels()).
channel(N, Channels) -> channel(N, read(top), Channels).
channel(N, H, Channels) ->
    B = dict:is_key(N, Channels),
    if
        B -> dict:fetch(N, Channels);
        true -> channel_helper(N, H)
    end.
channel_helper(N, finality) -> channels:read_channel(N);
channel_helper(N, H) ->
    X = read(H),
    Channels = X#x.channels,
    Parent = X#x.parent,
    case dict:find(N, Channels) of
        error -> channel_helper(N, Parent);
        {ok, Val} -> Val
    end.
backup(N) ->
   (N rem (fractions:multiply_int(constants:backup(), constants:max_reveal()))) == 0.
buy_block() -> buy_block(tx_pool:txs(), tx_pool:total_coins()).
buy_block(Txs, TotalCoins) -> buy_block(Txs, TotalCoins, 1).
buy_block(Txs, TotalCoins, BlockGap) ->
    ParentKey = read(top),
    ParentX = read(ParentKey),
    Parent = testnet_sign:data(ParentX#x.block),
    PHash = hash:doit(Parent),
    N = Parent#block.number + BlockGap,
    TcIncreases = to_channel_tx:tc_increases(Txs),
    CCLosses = channel_block_tx:cc_losses(Txs),
    RepoLosses = repo_tx:losses(Txs),
    CFLLosses = channel_funds_limit_tx:losses(Txs, dict:new(), read(top)),
    P = Parent#block.power + TcIncreases - CCLosses - RepoLosses - CFLLosses,
    Entropy = entropy:doit(N),
    Z = backup(N),
    %Z = N rem constants:finality(),
    DBR = if
	Z  -> backup:hash();
	true -> <<>>
    end,
    Block = #block{txs = Txs, hash = PHash, number = N, power = P, entropy = Entropy, total_coins = TotalCoins, db_root = DBR},
    absorb([keys:sign(Block)]).
sign_tx(Tx, Pub, Priv) -> sign_tx(Tx, Pub, Priv, 1).
sign_tx(Tx, Pub, Priv, N) -> testnet_sign:sign_tx(Tx, Pub, Priv, N, tx_pool:accounts()).
test() -> 
    %{Pub, Priv} = testnet_sign:new_key(),
    {Pub, Priv} = {<<"BFwMGotVNCqoUb6q14BlZhf+n2ZFyxOHl8LaFvfNNjSb/7+nkDFDfIGTDghc+Ozek98XcYer3ezoS2+TqICYrpw=">>, <<"apFy2hRE9lBRg+resDwJWbnUKjJtGbXShf0HKet37+8=">>},
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub, 620000, 0))),
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(1, 10, 0, keys:id()))),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    buy_block(),
    ABCDEF = keys:sign({sign_tx, 0, 0, 0, 0, 0, 0, 0}), 
    A3 = sign_tx(slasher_tx:slasher(1, ABCDEF), Pub, Priv),
    tx_pool_feeder:absorb(A3),
    CreateTx1 = keys:sign(to_channel_tx:create_channel(1, 110000, 10000, <<"delegated_1">>, 0)),
    SignedCreateTx1 = sign_tx(CreateTx1, Pub, Priv),
    true = testnet_sign:verify(SignedCreateTx1, tx_pool:accounts()),
    tx_pool_feeder:absorb(SignedCreateTx1),
    CreateTx2 = keys:sign(to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0)),
    SignedCreateTx2 = sign_tx(CreateTx2, Pub, Priv),
    tx_pool_feeder:absorb(SignedCreateTx2),
    CreateTx3 = keys:sign(to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0)),
    SignedCreateTx3 = sign_tx(CreateTx3, Pub, Priv),
    tx_pool_feeder:absorb(SignedCreateTx3),
    
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),
    %If we use the first A4, it should fail.
    %TTT = hd(tl(tl(tl(tl(element(5, testnet_sign:data(block_tree:read_int(2)))))))),
    %A4 = sign_tx(fork_slash_tx:slasher(1, TTT, 2), Pub, Priv),
    A4 = sign_tx(fork_slash_tx:slasher(1, keys:sign({sign_tx, 0, 0, 0, 0, 0, 1, 0}), 2), Pub, Priv),
    tx_pool_feeder:absorb(A4),
    ToChannel = keys:sign(to_channel_tx:to_channel(24000, 0, 10, 0)),
    SignedToChannel = sign_tx(ToChannel, Pub, Priv),
    tx_pool_feeder:absorb(SignedToChannel),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),%needs to start with some big channels with myself, so I have enough delegation.
    ChannelTx = channel_block_tx:close_channel(24000, -200, 1, 0),
    TimeoutTx = channel_block_tx:channel_block(24001, -200, 1, 0, 0),
    SlasherTx = channel_block_tx:channel_block(24002, -200, 1, 10, 0),
    SignedChannelTx = sign_tx(ChannelTx, Pub, Priv),
    SignedTimeoutTx = sign_tx(TimeoutTx, Pub, Priv),
    SignedSlasherTx = sign_tx(SlasherTx, Pub, Priv),
    Acc1 = account(1),
    A1 = accounts:balance(Acc1),
    tx_pool_feeder:absorb(keys:sign(channel_block_tx:make_signed_cb(keys:id(), SignedChannelTx, 0, []))),
    Acc2 = account(1),
    A2 = accounts:balance(Acc2),
    io:fwrite("A2: "),
    io:fwrite(integer_to_list(A2)),
    io:fwrite("\n"),
    io:fwrite("A1: "),
    io:fwrite(integer_to_list(A1)),
    io:fwrite("\n"),
    %true = A2 < A1,%4000 fee per block, only gain 1010.
    tx_pool_feeder:absorb(keys:sign(channel_timeout_tx:timeout_channel(keys:id(), SignedTimeoutTx))),
    tx_pool_feeder:absorb(keys:sign(channel_timeout_tx:timeout_channel(keys:id(), SignedSlasherTx))),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    buy_block(),
    tx_pool_feeder:absorb(keys:sign(channel_close_tx:slow_close(24001, keys:id()))),
    SlashBlock = channel_block_tx:channel_block(24002, 0, 2, 5, 0),
    SignedSlashBlock = sign_tx(SlashBlock, Pub, Priv),
    _AccOne = account(1),
    ChannelSlashTx = channel_slash_tx:make_tx(1, SignedSlashBlock, 0),
    %ChannelSlashTx = {channel_slash, 1, accounts:nonce(AccOne), SignedSlashBlock},
    SignedChannelSlashTx = sign_tx(ChannelSlashTx, Pub, Priv),
    %SignedChannelSlashTx = testnet_sign:sign_tx(ChannelSlashTx, Pub, Priv, 1, tx_pool:accounts()),
    tx_pool_feeder:absorb(SignedChannelSlashTx),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    %{Pub2, Priv2} = testnet_sign:new_key(),
    {Pub2, Priv2} = {<<"BOv0MdWGBReGf7b/DWQscSbhveCtv82uaJDHgfc5ZjP6CLM2hYcWnj0PNCZyIw8vMK133uyC4o2AxK5trA2uTIM=">>, <<"Zgn9fHaOCZTuFqUVUFCsSoNMxKOmSKQ6GIUB7YiTW9A=">>},
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub2, 650000, 0))),
    buy_block(),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    tx_pool_feeder:absorb(sign_tx(delete_account_tx:delete_account(2, 0, 0), Pub2, Priv2, 2)),
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub2, 680000, 0))),
    buy_block(),
    CreateTxq = keys:sign(to_channel_tx:create_channel(2, 110002, 0, <<"delegated_2">>, 0)),
    SignedCreateTxq = sign_tx(CreateTxq, Pub2, Priv2, 2),
    EmptyChannel = channels:empty(),
    true = EmptyChannel == channel(24000),
    tx_pool_feeder:absorb(SignedCreateTxq),
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(1, 1, 0, keys:id()))),
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(2, 1, 0, keys:id()))),
    false = EmptyChannel == channel(24000),
    CreateChannel01 = to_channel_tx:create_channel2(2, 1, 111000, 1000, <<"delegated_1">>, 0),
    SignedCreateChannel01 = sign_tx( sign_tx(CreateChannel01, Pub, Priv), Pub2, Priv2, 2),
    tx_pool_feeder:absorb(SignedCreateChannel01),
    CreateChannel02 = to_channel_tx:create_channel2(1, 2, 111000, 1000, <<"delegated_1">>, 0),
    SignedCreateChannel02 = sign_tx( sign_tx(CreateChannel02, Pub, Priv), Pub2, Priv2, 2),
    tx_pool_feeder:absorb(SignedCreateChannel02),
    %create 2 channels between the non-master accounts
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    buy_block(),
    ChannelHashlock01 = channel_block_tx:channel_block_from_channel(24001, channel(24001), 0, 1, 0, 0, []), %channel is a channels:acc1(Channel) type object.
    %channel_block_from_channel(Id, Channel, Amount, Nonce, Delay, Fee, Bets, tx_pool:accounts()).
    Secret = hash:doit(1),
    SH = hash:doit(Secret),
    SpendHashlock = language:run([Secret] ++ language:hashlock(SH), 1000),
    SpendHashlock = [2, {f, 1, 1}, {f, 0, 1}],
    SplitHashlock = language:run([hash:hash(<<"28">>)] ++ language:hashlock(SH), 1000),
    SplitHashlock = [1, {f, 0, 1}, {f, 0, 1}],
    ChannelHashlock11 = channel_manager:hashlock(24001, ChannelHashlock01, 40, SH, 1),
    SignedChannelHashlock11 = sign_tx( sign_tx(ChannelHashlock11, Pub, Priv), Pub2, Priv2, 2),
    SignedChannelHashlock21 = sign_tx(channel_timeout_tx:timeout_channel(2, testnet_sign:set_revealed(SignedChannelHashlock11, [[hash:doit(1)]])), Pub2, Priv2, 2),
    tx_pool_feeder:absorb(SignedChannelHashlock21),
    ChannelHashlock02 = channel_block_tx:channel_block_from_channel(24002, channel(24002), 0, 1, 0, 0, []), 
    ChannelHashlock12 = channel_manager:hashlock(24002, ChannelHashlock02, 30, SH, 1),
    SignedChannelHashlock12 = sign_tx( sign_tx(ChannelHashlock12, Pub, Priv), Pub2, Priv2, 2),
    SignedChannelHashlock22 = sign_tx(channel_timeout_tx:timeout_channel(2, testnet_sign:set_revealed(SignedChannelHashlock12, [[hash:doit(1)]])), Pub2, Priv2, 2),
    %Two problems here. 
    %First off, the bet is only changing the balance by 15, not 30. Probably language:hashlock needs to change a fraction from 1/2 to 1/1.
    %Money is being printed from no where.
    tx_pool_feeder:absorb(SignedChannelHashlock22),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    buy_block(),
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(1, 1, 0, keys:id()))),
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(2, 1, 0, keys:id()))),
    B2B0 = accounts:balance(account(1)),
    A2B0 = accounts:balance(account(2)),
    CloseHashlock01 = channel_close_tx:slow_close(24001, 2),
    CloseHashlock11 = sign_tx(CloseHashlock01, Pub2, Priv2, 2),
    tx_pool_feeder:absorb(CloseHashlock11),
    B2B1 = accounts:balance(account(1)),
    A2B1 = accounts:balance(account(2)),
    CloseHashlock02 = channel_close_tx:slow_close(24002, 2),
    CloseHashlock12 = sign_tx(CloseHashlock02, Pub2, Priv2, 2),
    tx_pool_feeder:absorb(CloseHashlock12),
    B2B2 = accounts:balance(account(1)),
    A2B2 = accounts:balance(account(2)),
    A2B2a = 111040, %was 110960
    A2B2a = A2B1 - A2B0,
    A2B2b = 1030,%was 970
    A2B2b = A2B2 - A2B1,
    B2B2a = 960, %was 1040
    B2B2a = B2B1 - B2B0,
    B2B2b = 110970, %was 111030
    B2B2b = B2B2 - B2B1,
    %begin spending money in each channel.
    %close one of the two channels with the bet in it. Unlock the bet.
    %make sure the right new balance is put into our account.
    %close one of the two channels with the bet in it. leave the bet locked.
    %make sure the right new balance is put into our account.
    success.
long_test() -> 
    %{Pub, Priv} = testnet_sign:new_key(),
    {Pub, Priv} = {<<"BFwMGotVNCqoUb6q14BlZhf+n2ZFyxOHl8LaFvfNNjSb/7+nkDFDfIGTDghc+Ozek98XcYer3ezoS2+TqICYrpw=">>, <<"apFy2hRE9lBRg+resDwJWbnUKjJtGbXShf0HKet37+8=">>},
    Addr = testnet_sign:pubkey2address(Pub),
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Addr, 620000, 0))),
    tx_pool_feeder:absorb(keys:sign(spend_tx:spend(1, 10, 0, keys:id()))),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),
    tx_pool_feeder:absorb(sign_tx(slasher_tx:slasher(1, keys:sign({sign_tx, 0, 0, 0, 0, 0, 0, 0})), Pub, Priv)),
    %Top = read(read(top)),
    CreateTx1 = keys:sign(to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0)),
    SignedCreateTx1 = sign_tx(CreateTx1, Pub, Priv),
    tx_pool_feeder:absorb(SignedCreateTx1),
    CreateTx2 = keys:sign(to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0)),
    SignedCreateTx2 = sign_tx(CreateTx2, Pub, Priv),
    tx_pool_feeder:absorb(SignedCreateTx2),
    CreateTx3 = keys:sign(to_channel_tx:create_channel(1, 110000, 1000, <<"delegated_1">>, 0)),
    SignedCreateTx3 = sign_tx(CreateTx3, Pub, Priv),
    tx_pool_feeder:absorb(SignedCreateTx3),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),
    %Top2 = read(read(top)),
    ToChannel = keys:sign(to_channel_tx:to_channel(24000, 0, 10, 0)),
    SignedToChannel = sign_tx(ToChannel, Pub, Priv),
    tx_pool_feeder:absorb(SignedToChannel),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),%needs to start with some big channels with myself, so I have enough delegation.
    %Top3 = read(read(top)),
    ChannelTx = keys:sign(channel_block_tx:close_channel(24000, -200, 1, 0)),
    TimeoutTx = keys:sign(channel_block_tx:channel_block(24001, -200, 1, 0, 0)),
    SlasherTx = channel_block_tx:channel_block(24002, -200, 1, 10, 0),
    SignedChannelTx = sign_tx(ChannelTx, Pub, Priv),
    SignedTimeoutTx = sign_tx(TimeoutTx, Pub, Priv),
    SignedSlasherTx = sign_tx(SlasherTx, Pub, Priv),
    Acc1 = account(1),
    A1 = accounts:balance(Acc1),
    tx_pool_feeder:absorb(keys:sign(channel_block_tx:make_signed_cb(keys:id(), SignedChannelTx, 0, []))),
    Acc2 = account(1),
    A2 = accounts:balance(Acc2),
    true = A2 < A1,%4000 fee per block, only gain 1010.
    tx_pool_feeder:absorb(keys:sign(channel_timeout_tx:timeout_channel(keys:id(), keys:sign(SignedTimeoutTx)))),
    tx_pool_feeder:absorb(keys:sign(channel_timeout_tx:timeout_channel(keys:id(), keys:sign(SignedSlasherTx)))),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),
    tx_pool_feeder:absorb(keys:sign(channel_close_tx:slow_close(24001, keys:id()))),
    SlashBlock = channel_block_tx:channel_block(24002, 0, 2, 5, 0),
    SignedSlashBlock = sign_tx(SlashBlock, Pub, Priv),
    ChannelSlashTx = channel_slash_tx:make_tx(1, SignedSlashBlock, 0),
    SignedChannelSlashTx = sign_tx(ChannelSlashTx, Pub, Priv),
    tx_pool_feeder:absorb(SignedChannelSlashTx),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    buy_block(),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))),
    %{Pub2, Priv2} = testnet_sign:new_key(),
    {Pub2, Priv2} = {<<"BOv0MdWGBReGf7b/DWQscSbhveCtv82uaJDHgfc5ZjP6CLM2hYcWnj0PNCZyIw8vMK133uyC4o2AxK5trA2uTIM=">>, <<"Zgn9fHaOCZTuFqUVUFCsSoNMxKOmSKQ6GIUB7YiTW9A=">>},
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub2, 650000, 0))),
    buy_block(),
    tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    tx_pool_feeder:absorb(sign_tx(delete_account_tx:delete_account(2, 0, 0), Pub2, Priv2, 2)),
    tx_pool_feeder:absorb(keys:sign(create_account_tx:create_account(Pub2, 680000, 0))),
    buy_block(),
    CreateTxq = keys:sign(to_channel_tx:create_channel(2, 110000, 0, <<"delegated_2">>, 0)),
    SignedCreateTxq = sign_tx(CreateTxq, Pub2, Priv2, 2),
    EmptyChannel = channels:empty(),
    true = EmptyChannel == channel(24000),
    tx_pool_feeder:absorb(SignedCreateTxq),
    false = EmptyChannel == channel(24000),
    %tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))),
    F = fun() -> tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))), buy_block() end,
    G = fun() -> F(), F(), F(), F(), F(), F(), F(), F() end,
    H = fun() -> G(), G(), G(), G(), G(), G(), G(), G() end,
    H(),
    SHtestslashed = sign_tx:secret_hash(testnet_sign:data(hd(tl(tl(block_tree:block2txs(testnet_sign:data(block_finality:read(1)))))))),
    false = block_tree:secret(0, SHtestslashed, block_tree:read(top), dict:new()),
    SHtest = sign_tx:secret_hash(testnet_sign:data(hd(block_tree:block2txs(testnet_sign:data(block_finality:read(10)))))),
    true = block_tree:secret(9, SHtest, block_tree:read(top), dict:new()),
    true = all_secrets:exists(9, SHtest),%because block 10 contains signatures over block 9.
    D1a = accounts:delegated(account(0)),
    D2a = accounts:delegated(account(1)),
    %to_channel_tx:create_channel(1, 10000, 1000, non_delegated, 0),
    %to_channel_tx:create_channel(1, 10000, 1000, delegated_2, 0),
    F2 = fun() -> 
		 tx_pool_feeder:absorb(keys:sign(sign_tx:sign(keys:id()))), 
		 R = reveal:reveal(keys:id()),
		 if
		     is_atom(R) -> ok;
		     true -> 
			 tx_pool_feeder:absorb(keys:sign(R))
		 end,
		 %tx_pool_feeder:absorb(keys:sign(reveal:reveal(keys:id()))), 
		 buy_block(), 
		 timer:sleep(200) end,
    G2 = fun() -> F2(), F2(), F2(), F2(), F2(), F2(), F2(), F2() end,
    H2 = fun() -> G2(), G2(), G2(), G2(), G2(), G2(), G2(), G2() end,
    H2(),
    false = all_secrets:exists(9, SHtest),
    D1b = accounts:delegated(account(0)),
    D2b = accounts:delegated(account(1)),
    D2a = D2b,
    D1a = D1b,
    0 = accounts:delegated(block_tree:account(1)),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    H2(),
    %H2(),
    %H2(),
    %This next test only works if account 2 is low enough on money. If some constants were changed, then we may need to run H2() more times before the next step.
    EmptyAccount = accounts:empty(),
    Acc2a = account(2),
    false = EmptyAccount == Acc2a,
    timer:sleep(5000),
    tx_pool_feeder:absorb(keys:sign(channel_funds_limit_tx:make_tx(24000, 0, keys:id()))),
    tx_pool_feeder:absorb(keys:sign(repo_tx:repo(2, 0, [], keys:id()))),
    Acc2b = account(2),
    true = EmptyAccount == Acc2b,
    success.

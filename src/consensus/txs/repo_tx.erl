%similar to slasher.
%Each account needs a minimum amount of money.
%If you can provide evidence that someone doesn't have enough money left to validate, you can take some of their money, which simultaniously deletes all their delegation, and changes the consensus_flag in the channels to off.

-module(repo_tx).
-export([doit/7, repo/4, losses/1]).
-record(repo, {acc = 0, nonce = 0, target = 0, fee = 0, delegated = 0, channels = []}).
repo(Target, Fee, Channels, Acc) -> %Acc is keys:id()
    A = block_tree:account(Acc),
    T = block_tree:account(Target),
    true = low_balance(T, block_tree:total_coins(), block_tree:height()),
    Nonce = accounts:nonce(A),
    #repo{acc = Acc, nonce = Nonce + 1, target = Target, fee = Fee, channels = Channels, delegated = accounts:delegated(T)}.
low_balance(Acc, TotalCoins, NewHeight) -> 
    UCost = accounts:unit_cost(Acc, TotalCoins),
    MinBalance = UCost * constants:max_reveal(),
    Gap = NewHeight - accounts:height(Acc),
    Cost = UCost * Gap,
    NewBalance = accounts:balance(Acc) - Cost,
    MinBalance div 2 > NewBalance.
un_delegate([], Channels) -> Channels;
un_delegate([ID|T], Channels) -> 
    %Ch = block_tree:channel(ID, ParentKey, Channels),
    NewChannels = dict:store(ID, channels:empty(), Channels),
    un_delegate(T, NewChannels).
%all_channels(Amount, _, _, _, _) when Amount < 0 -> 0 = 1;
all_channels(0, _, _, _, []) -> true;
all_channels(0, _, _, _, _) -> false;
all_channels(Amount, _, _, _, []) when Amount > 0 -> false;
all_channels(Amount, Accn, Channels, ParentKey, [Chn|Chs]) -> 
    true = Amount > 0,
    Channel = block_tree:channel(Chn, ParentKey, Channels),
    N = channels:bal1(Channel) + channels:bal2(Channel),
    Accn = case channels:type(Channel) of
	<<"delegated_1">> -> channels:acc1(Channel);
	<<"delegated_2">> -> channels:acc2(Channel)
    end,
    all_channels(Amount - N, Accn, Channels, ParentKey, Chs).
losses(Txs) -> losses(Txs, 0).
losses([], X) -> X;
losses([SignedTx|Txs], X) -> 
    Tx = testnet_sign:data(SignedTx),
    if
	is_record(Tx, repo) ->
	    losses(Txs, X + Tx#repo.delegated);
	true -> losses(Txs, X)
    end.
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    Acc = Tx#repo.acc,
    Target = Tx#repo.target,
    A = block_tree:account(Acc, ParentKey, Accounts),
    T = block_tree:account(Target, ParentKey, Accounts),
    true = low_balance(T, TotalCoins, NewHeight),
    D = accounts:delegated(T),
    D = Tx#repo.delegated,
    true = all_channels(D, Target, Channels, ParentKey, Tx#repo.channels),
    %the Tx also needs to list every channel that they are delegated for, so we can delete those too.
    KT = 3,%deletes (KT - 1) / (KT) of their balance, and gives rest as reward.
    Keep = accounts:balance(T) div KT,
    NA = accounts:update(A, NewHeight, constants:delete_account_reward() + Keep - Tx#repo.fee, 0, 1, TotalCoins),
    Nonce = accounts:nonce(NA),
    Nonce = Tx#repo.nonce,
    Accounts2 = dict:store(Acc, NA, Accounts),
    Accounts3 = dict:store(Target, accounts:empty(), Accounts2),
    %we need to change the delegation flag of each channel to non_delegated.
    NewChannels = un_delegate(Tx#repo.channels, Channels),
    {NewChannels, Accounts3, TotalCoins + constants:delete_account_reward() - (Keep * (KT - 1)), S}.

%problem, this changes the power of the block...

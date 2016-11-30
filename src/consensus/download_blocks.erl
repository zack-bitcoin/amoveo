-module(download_blocks).
-export([sync/2, absorb_txs/1]).

%download_blocks:sync({127,0,0,1}, 3020).
sync(IP, Port) ->
    {ok, TheirHeight} = talker:talk({height}, IP, Port),
    MyHeight = block_tree:height(),
    MH = MyHeight + constants:max_reveal(),
    if
        TheirHeight > MH ->
            fresh_sync(IP, Port, TheirHeight);
        TheirHeight > MyHeight ->
            get_blocks(MyHeight + 1, TheirHeight, IP, Port);
	MyHeight > TheirHeight ->
	    give_blocks(TheirHeight + 1, MyHeight, IP, Port);
	true -> 1 = 1
    end,
    get_txs(IP, Port).
    

get_starter_block(IP, Port, Height) ->
    %keep walking backward till we get to a block that has a backup hash...
    Z = block_tree:backup(Height),
    false = Height < 0,
    if
	Z -> talker:talk({block, Height}, IP, Port);
	true -> get_starter_block(IP, Port, Height - 1)
    end.
	     
absorb_stuff([], _, _) -> ok;
absorb_stuff([File|T], IP, Port) ->
    %should download files.
    %File is an atom, which cannot be packed with packer...
    {ok, Size} = talker:talk({backup_size, File}, IP, Port),
    {ok, F} = file:open(File, [binary, raw, write, read]),
    absorb2(File, F, 0, Size, IP, Port),
    file:close(F),
    file:copy(File, "backup/"++File),
    absorb_stuff(T, IP, Port). 
absorb2(_, _, Step, Size, _, _) when Step > Size -> ok;
absorb2(FileName, File, Step, Size, IP, Port) ->
    {ok, Chunk} = talker:talk({backup_read, FileName, Step}, IP, Port),
    file:pwrite(File, Step * constants:word_size(), Chunk),
    absorb2(FileName, File, Step + 1, Size, IP, Port).

fresh_sync(IP, Port, PeerData) ->
    TheirHeight = PeerData,
    Z = fractions:multiply_int(constants:backup(), constants:max_reveal()),
    MyHeight = block_tree:height(),
    if 
	TheirHeight < Z -> 
	    get_blocks(MyHeight + 1, TheirHeight, IP, Port);
	true ->
	    {ok, SignedBlock} = get_starter_block(IP, Port, TheirHeight),
	    Block = testnet_sign:data(SignedBlock),
	    N = block_tree:block_number(Block),
	    block_pointers:set_start(N - constants:max_reveal() - 2),
	    DBRoot = block_tree:block_root(Block),
	    absorb_stuff(backup:backup_files(), IP, Port),
	    all_secrets:reset(),
	    accounts:reset(),
	    channels:reset(),
	    DBRoot = backup:hash(),
	    blocks_to_finality(N - constants:max_reveal() - 1, N - constants:finality() - 1, IP, Port),
	    block_tree:reset(),
	    get_blocks(N - constants:finality() - 1, TheirHeight, IP, Port)
    end,
    0.
    %starting from recent block, walk backward to find the backup hash.
    %download the files, and check that they match the backup hash.
    %load the blocks in from oldest to newest.

blocks_to_finality(Start, Finish, _, _) when Start>Finish ->ok;
blocks_to_finality(Start, Finish, IP, Port) ->
    {ok, SignedBlock} = talker:talk({block, Start}, IP, Port),
    block_finality:append(SignedBlock, Start),
    blocks_to_finality(Start + 1, Finish, IP, Port).
give_blocks(Start, Finish, _, _) when Start>Finish -> ok;
give_blocks(Start, Finish, IP, Port) ->
    Block = block_tree:read_int(Start),
    T = case talker:talk({give_block, Block}, IP, Port) of
	    {ok, 0} -> 1;
	    {error, _} -> 0
	end,
    give_blocks(Start + T, Finish, IP, Port).
		   
get_blocks(Start, Finish, _, _) when Start>Finish -> ok;
get_blocks(Start, Finish, IP, Port) ->
    {ok, SignedBlock} = talker:talk({block, Start}, IP, Port),
    block_tree:absorb([SignedBlock]),
    get_blocks(Start + 1, Finish, IP, Port).
absorb_txs([]) -> ok;
absorb_txs([Tx|T]) -> 
    %io:fwrite("attempt to absorb tx "),
    %io:fwrite(packer:pack(Tx)),
    %io:fwrite("\n"),
    tx_pool_feeder:absorb(Tx),
    absorb_txs(T).
get_txs(IP, Port) ->
    case talker:talk({txs}, IP, Port) of
	{error, timeout} -> 
	    timer:sleep(200),
	    get_txs(IP, Port);
	{ok, Txs} ->
	    MyTxs = tx_pool:txs(),
	    absorb_txs(flip(set_minus(Txs, MyTxs))),
	    Respond = flip(set_minus(MyTxs, Txs)),
	    if
		length(Respond) > 0 ->
		    talker:talk({txs, Respond}, IP, Port);
		true -> ok
	    end
    end,
    ok.
set_minus(A, B) -> set_minus(A, B, []).
set_minus([], _, Out) -> Out;
set_minus([A|T], B, Out) ->
    C = is_in(A, B),
    if
	C -> set_minus(T, B, Out);
	true -> set_minus(T, B, [A|Out])
    end.
is_in(_, []) -> false;
is_in(A, [A|_]) -> true;
is_in(A, [_|T]) -> is_in(A, T).
    
	    
flip(X) -> flip(X, []).
flip([], X) -> X;
flip([H|T], X) -> flip(T, [H|X]).
		    
    
    


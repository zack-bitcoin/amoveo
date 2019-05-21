-module(block_db).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/1, read/2, write/2,
         read_by_height/1,
         uncompress/1, compress/1,
         check/0, by_height_from_compressed/2,
         ram_height/0, genesis/0,
         test/0]).
-include("../../records.hrl").
-define(LOC, constants:block_db_dict()).
-define(LOC2, constants:block_db_dict2()).
-define(blocks_loc, constants:blocks_file2()).
-define(version, 2).%1 stores the recent blocks in a dictionary, 2 stores them in ets.
%-define(ram_limit, 20000000).%20 megabytes
%-define(ram_limit, 200000).%200 kilobytes
%-define(ram_limit, 10000).
-record(d, {dict = dict:new(), 
            many_blocks = 0,
            page_number = 0,
            pages = dict:new(),
            ram_bytes = 0,
            hd_bytes = 0,
            blocks_hd,
            ram_height = 0,%the lowest height block in ram before we switch to storing on the hard drive.
            genesis %the genesis block
           }).
init(ok) ->
     
    %io:fwrite("start block_db\n"),
    process_flag(trap_exit, true),
        {ok, F} = file:open(?blocks_loc, [write, read, raw, binary]),
        X = db:read(?LOC),
        Ka = if
                      X == "" ->
                      #d{};
                      true -> X
                                   end,
    case ets:info(?MODULE) of
        undefined ->
            case ets:file2tab(?LOC2) of
                {ok, ?MODULE} -> ok;
                {error, _} ->
                    ets:new(?MODULE, [set, named_table, compressed])
            end;
        _ -> ok
    end,
    K2 = Ka#d{blocks_hd = F},
    {ok, K2}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    ets:tab2file(?MODULE, ?LOC2, [{sync, true}]),
    file:close(X#d.blocks_hd),
    X2 = X#d{blocks_hd = 0},
    db:save(?LOC, X2),
    io:format("block_db died!"), 
    ok.
%ets:insert(ID, {key, value})
%ets:delete(ID, Key)
%ets:lookup(ID, Key) -> [] or [{Key, Value}]
erase(Key, D) ->
    case ?version of
        1 -> dict:erase(Key, D);
        2 -> ets:delete(?MODULE, Key),
             ok
    end.
            
lookup(K, D) ->
    case ?version of
        1 -> dict:find(K, D);
        2 -> 
            case ets:lookup(?MODULE, K) of
                [] -> error;
                %[{K, V}] -> {ok, uncompress(V)}
                [{K, V}] -> {ok, V}
            end
    end.
fetch(K, D) ->
    element(2, lookup(K, D)).
fetch_keys(D) ->
    case ?version of
        1 -> dict:fetch_keys(D);
        2 -> 
            fetch_keys2(ets:tab2list(?MODULE))
    end.
fetch_keys2([]) -> [];
fetch_keys2([{K, _}|T]) -> 
    [K|fetch_keys2(T)].
store(K, V, D)->
    case ?version of
        1 -> dict:store(K, V, D);
        %2 -> ets:insert(?MODULE, {K, compress(V)}),
        2 -> ets:insert(?MODULE, {K, V}),
             ok
    end.
            
            
handle_info(_, X) -> {noreply, X}.
handle_cast({write, Block, Hash}, X) -> 
    S = size(erlang:term_to_binary(Block)),
    D2 = store(Hash, Block, X#d.dict),
    X2 = X#d{dict = D2, ram_bytes = X#d.ram_bytes + S, many_blocks = X#d.many_blocks + 1},
    X4 = case element(2, Block) of
             0 -> X2#d{genesis = Block};
             _ -> X2
         end,
    X3 = check_compress(X4),
    %most of the time, we just add the block to the dict.
    %if the dict has become large enough, then we should gather up a bunch of blocks to compress onto the hard drive, and replace each block in the dict with a pointer to the new file.
    {noreply, X3};
handle_cast(_, X) -> {noreply, X}.
handle_call(genesis, _From, X) -> 
    {reply, X#d.genesis, X};
handle_call(ram_height, _From, X) -> 
    {reply, X#d.ram_height, X};
handle_call({read, Hash}, _From, X) -> 
    D = X#d.dict,
    R = case lookup(Hash, D) of
            error -> error;
            {ok, N} ->
                if
                    is_integer(N) -> %block on the hard drive.
                        {Loc, Size, _, _} = dict:fetch(N, X#d.pages),
                        {ok, Data} = read_page(Loc, Size, X#d.blocks_hd),
                        P = uncompress(Data),
                        dict:fetch(Hash, P);
                    true -> N %a block in ram
                end
        end,
    {reply, R, X};
handle_call({read, Many, Height, B}, _From, X) 
  when (Height >= X#d.ram_height) -> 
    H = block:height(),
    H2 = min(H, Height + Many),
    M = max(1, 1 + H2 - Height),
    %B = block:get_by_height(X),
    BH = block:prev_hash(0, B),
    RH = X#d.ram_height,
    Y = lists:reverse([B|read_dict2(M, BH, X, RH)]),
    %Y = [B|read_dict2(M, BH, X#d.dict, RH)],
    {reply, {ok, Y}, X};
handle_call({read_by_height, Height}, _From, X) ->
    Page = case find_page_loc(Height, X#d.pages, 0) of
               {Loc, Size} -> 
                   {ok, Y} = read_page(Loc, Size, X#d.blocks_hd),
                   Y;
               error -> error
           end,
    {reply, Page, X};
handle_call({read, _, _Height, B}, _From, X) ->
    %BH = block:prev_hash(0, B),
    BH = block:hash(B),
    P = case lookup(BH, X#d.dict) of
            error -> error;
            {ok, N} -> 
                {Loc, Size} = dict:fetch(N, X#d.pages),
                read_page(Loc, Size, X#d.blocks_hd)
        end,
    {reply, P, X};
handle_call(_, _From, X) -> {reply, X, X}.

find_page_loc(Height, Pages, N) ->
   case dict:find(N, Pages) of
       error -> error;
       {ok, {Loc, Size, Start, End}} ->
           if
               ((Height > (Start - 1)) and
                (Height < (End + 1))) ->
                   {Loc, Size};
               true -> find_page_loc(Height, Pages, N+1)
           end
   end.
    

read_page(Loc, Size, File) ->
    file:pread(File, Loc, Size).
write_page(Dict, X) ->
    B = X#d.hd_bytes,
    Data = compress(Dict),
    S = size(Data),
    file:pwrite(X#d.blocks_hd, B, Data),
    {B, S}.

compress(X) ->
    S = zlib:open(),
    {ok, CL} = application:get_env(amoveo_core, compression_level),
    zlib:deflateInit(S, CL),
    B1 = zlib:deflate(S, term_to_binary(X)),
    B2 = zlib:deflate(S, <<>>, finish),
    zlib:close(S),
    %zlib:compress(term_to_binary(X)).
    list_to_binary([B1, B2]).
uncompress(X) ->
    %io:fwrite(X),
    S = zlib:open(),
    zlib:inflateInit(S),
    Y = binary_to_term(list_to_binary(zlib:inflate(S, X))),
    zlib:close(S),
    Y.
    %binary_to_term(zlib:uncompress(X)).
    
            

% * starting from the head, walk backwards FT steps. everything earlier than that block is going to the hd.
old_blocks(Head, 0, D) ->
    old_blocks2(dict:new(), 0, Head, D);
old_blocks(Head, ForkTolerance, D) ->
    B = fetch(Head, D),
    BH = block:prev_hash(0, B),
    old_blocks(BH, ForkTolerance - 1, D).
old_blocks2(Blocks, Size, Head, D) ->
    case lookup(Head, D) of
        error -> {Blocks, Size};
        {ok, Block} ->
            if
                is_integer(Block) -> {Blocks, Size};
                true ->
                    Block2 = Block#block{prev_hashes = 0},
                    old_blocks2(dict:store(Head, Block2, Blocks),
                                Size + size(term_to_binary(Block2)),
                                block:prev_hash(0, Block),
                                D)
            end
    end.
height_range(Blocks) ->%page of blocks
    K = dict:fetch_keys(Blocks),
    false = (K == []),
    hr2(K, Blocks, block:height(), 0).
hr2([], _, Start, End) -> {Start, End};
hr2([H|T], Blocks, S, E) ->
    B = fetch(H, Blocks),
    Height = B#block.height,
    hr2(T, Blocks, min(S, Height), max(E, Height)).
    
check_compress(X) ->
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    {ok, RL} = application:get_env(amoveo_core, block_cache),
    %FT = 2,
    RB = X#d.ram_bytes,
    if
        ((RB > RL) and 
         (X#d.many_blocks > (FT * 2)))->
            TH = block:hash(headers:top_with_block()),
            {Blocks, _} = old_blocks(TH, FT, X#d.dict),%gather up a dict of all the blocks we are moving to the hd.
            {Loc, Size} = write_page(Blocks, X),
            %Start = (hd(Blocks))#block.height,
            {Start, End} = height_range(Blocks),
            %Start = X#d.ram_height,
            %End = block:height(),
            %io:fwrite("compressed loc: "),
            %io:fwrite(packer:pack([Loc, Size])),
            %io:fwrite(" many blocks: "),
            %io:fwrite(packer:pack([X#d.many_blocks])),
            %io:fwrite("\n"),
            %End = (hd(lists:reverse(Blocks)))#block.height,
            PageNumber = X#d.page_number,
            Pages = dict:store(PageNumber, {Loc, Size, Start, End}, X#d.pages),
            BK = dict:fetch_keys(Blocks),
            Dict2 = replace_block_with_page(BK, PageNumber, X#d.dict), %replace many blocks in dict with their page number.
            %delete any old uncles from dict, and reduce ram_bytes accordingly.
            FK = fetch_keys(Dict2),
            {ManyUncles, Dict3} = remove_uncles_before(block:height() - FT, FK, Dict2, 0),%here
            %io:fwrite([X#d.many_blocks, BK, ManyUncles]),
            X#d{dict = Dict3, 
                pages = Pages, 
                page_number = PageNumber + 1, 
                %ram_bytes = RB - Bytes,
                ram_bytes = 0,
                many_blocks = X#d.many_blocks - length(BK) - ManyUncles,
                ram_height = block:height() - FT,
                hd_bytes = X#d.hd_bytes + Size};
        true -> X
    end.
replace_block_with_page([], _, D) -> D;
replace_block_with_page([H|T], N, D) ->
    D2 = store(H, N, D),
    replace_block_with_page(T, N, D2).
remove_uncles_before(_, [], D, U) -> {U, D};
remove_uncles_before(Height, [H|T], D, U) ->
    B = fetch(H, D),
    if
        is_integer(B) -> 
            remove_uncles_before(Height, T, D, U);
        true ->
            BH = element(2, B),
            {U2, D2} = if
                           Height > BH -> 
                               {U+1, erase(H, D)};
                           true -> {U, D}
                       end,
            remove_uncles_before(Height, T, D2, U2)
    end.
read_dict2(0, _, _, _) -> [];
read_dict2(N, BH, X, RH) ->
    D = X#d.dict,
    case lookup(BH, D) of
        error -> [];
        {ok, B} ->
            if
                is_tuple(B) ->
                    Height = element(2, B),
                    case Height of
                        0 -> [B];
                        RH -> [B];
                        _ ->
                            BH2 = block:prev_hash(0, B),
                            [B|read_dict2(N-1, BH2, X, RH)]
                    end;
                is_integer(B) -> [];
                %false ->
                %    {Loc, Size, Start, End} = dict:fetch(B, X#d.pages),
                %    {ok, CPage} = read_page(Loc, Size, X#d.blocks_hd),
                %    PDict = uncompress(CPage),
                %    L0 = sync:low_to_high(sync:dict_to_blocks(dict:fetch_keys(PDict), PDict)),
                %    LBH = element(2, hd(L0)),
                %    {_, L} = lists:split( min(RH-LBH-1, length(L0)), L0),
                %    lists:reverse(L);
                true ->
                    %io:fwrite(B),
                    []
                    %{Loc, Size} = dict:fetch(N, X#d.pages),
                    %{ok, CPage} = read_page(Loc, Size, X#d.blocks_hd),
                    %Page = uncompress(CPage)
                    %[B]
                %io:fwrite(B)
            end
    end.
            

test() ->
    NewDict = dict:new(),
    {ok, F} = file:open(?blocks_loc, [write, read, raw, binary]),
    X = #d{blocks_hd = F},
    {Loc, Size, _, _} = write_page(NewDict, X),
    {ok, Data} = read_page(Loc, Size, X#d.blocks_hd),
    uncompress(Data).
    













by_height_from_compressed(D, N) ->
    %io:fwrite("block_db by height from compressed"),
    %io:fwrite(integer_to_list(N)),
    %io:fwrite("\n"),
    %N = 0,
    D2 = uncompress(D),
    K = dict:fetch_keys(D2),
    bhfc2(K, D2, N).
bhfc2([H|T], D, N) ->
    A = dict:fetch(H, D),
    if
        (A#block.height == N) -> A;
        true -> bhfc2(T, D, N)
    end.
            

read_by_height(Height) ->
    %HN = block:height(),
    %{ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    RH = ram_height(),
    if
        Height < RH ->
            %true = Height < RH,
            true = Height > -1,
            gen_server:call(?MODULE, {read_by_height, Height});
        true -> block:get_by_height(Height)
    end.
ram_height()  ->
    gen_server:call(?MODULE, ram_height).
genesis()  ->
    gen_server:call(?MODULE, genesis).
    
read(Many, Height) ->
    {ok, Version} = application:get_env(amoveo_core, db_version),
    case Version of
        1 ->
            H = block:height(),
            X = min(H, Height + Many),
            M = max(1, 1 + X - Height),
            B = block:get_by_height(X),
            BH = block:prev_hash(0, B),
            lists:reverse([B|read2(M, BH)]);
        _ ->
            %io:fwrite("block db read/2 "),
            %io:fwrite(packer:pack([Many, Height])),
            %io:fwrite("\n"),
            H = block:height(),
            %{ok, FT} = application:get_env(amoveo_core, fork_tolerance),
            RH = ram_height(),
            if
                (Height < RH) -> 
                    %io:fwrite("read ram block\n"),
                    read_by_height(Height);
                true ->
                    %io:fwrite("read hd block\n"),
                    X = min(H, Height + Many),
                    Block = block:get_by_height(X),
                    {ok, Z} = gen_server:call(?MODULE, {read, Many, Height, Block}),
                    Z
            end
    end.
read2(0, _) -> [];
read2(N, BH) ->
    B = read(BH),
    Height = element(2, B),
    case Height of
        0 -> [B];
        _ ->
            BH2 = block:prev_hash(0, B),
            [B|read2(N-1, BH2)]
    end.
    
read(Hash) ->
    {ok, Version} = application:get_env(amoveo_core, db_version),
    case Version of
        1 ->
            BlockFile = binary_to_file_path(blocks, Hash),
            case db:read(BlockFile) of
                [] -> empty;
                Block -> uncompress(Block)
            end;
        _ ->
            gen_server:call(?MODULE, {read, Hash})
    end.

write(Block, Hash) ->
    {ok, Version} = application:get_env(amoveo_core, db_version),
    case Version of
        1 ->
            CompressedBlockPlus = compress(Block),
                                                %Hash = block:hash(Block),
            BlockFile = binary_to_file_path(blocks, Hash),
            ok = db:save(BlockFile, CompressedBlockPlus);
        _ ->
            gen_server:cast(?MODULE, {write, Block, Hash})
    end.
check() ->
    gen_server:call(?MODULE, check).
    

binary_to_file_path(Code, Binary) ->
    Code = blocks,
    <<Byte, _/binary>> = Binary,
    H = to_hex(<<Byte>>),
    Encoded = base58:binary_to_base58(Binary),
    Dir = file_dir(Code),
    Dir ++ H ++ "/" ++ Encoded ++ ".db".
    %Dir ++ Encoded ++ ".db".

file_dir(blocks) -> constants:blocks_file();%"blocks/";
file_dir(oracle_questions) -> constants:oracle_questions_file().%"oracle_questions/".

to_hex(<<>>) ->  [];
to_hex(<<A:4, B/bitstring>>) ->
    if
        A < 10 -> [(A+48)|to_hex(B)];
        true -> [(A+87)|to_hex(B)]
    end.

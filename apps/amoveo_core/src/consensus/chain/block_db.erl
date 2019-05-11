-module(block_db).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/1, read/2, write/2,
         uncompress/1, compress/1,
         check/0,
         test/0]).
-define(LOC, constants:block_db_dict()).
-define(blocks_loc, constants:blocks_file2()).
-define(ram_limit, 20000000).%20 megabytes
%-define(ram_limit, 10000).
-define(version, old).
-record(d, {dict = dict:new(), 
            many_blocks = 0,
            page_number = 0,
            pages = dict:new(),
            ram_bytes = 0,
            hd_bytes = 0,
            blocks_hd,
            ram_height = 0%the lowest height block in ram before we switch to storing on the hard drive.
           }).
init(ok) -> 
    io:fwrite("start block_db\n"),
    process_flag(trap_exit, true),
    {ok, F} = file:open(?blocks_loc, [write, read, raw, binary]),
    X = db:read(?LOC),
    Ka = if
	     X == "" -> #d{};
	     true -> X
	 end,
    K2 = Ka#d{blocks_hd = F},
    {ok, K2}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    file:close(X#d.blocks_hd),
    X2 = X#d{blocks_hd = 0},
    db:save(?LOC, X2),
    io:format("block_db died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({write, Block, Hash}, X) -> 
    S = size(erlang:term_to_binary(Block)),
    D2 = dict:store(Hash, Block, X#d.dict),
    X2 = X#d{dict = D2, ram_bytes = X#d.ram_bytes + S, many_blocks = X#d.many_blocks + 1},
    X3 = check_compress(X2),
    %most of the time, we just add the block to the dict.
    %if the dict has become large enough, then we should gather up a bunch of blocks to compress onto the hard drive, and replace each block in the dict with a pointer to the new file.
    {noreply, X3};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Hash}, _From, X) -> 
    D = X#d.dict,
    R = case dict:find(Hash, D) of
            error -> error;
            {ok, N} ->
                if
                    is_integer(N) -> %block on the hard drive.
                        {Loc, Size} = dict:fetch(N, X#d.pages),
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
    Y = lists:reverse([B|read_dict2(M, BH, X#d.dict, RH)]),
    %Y = [B|read_dict2(M, BH, X#d.dict, RH)],
    {reply, Y, X};
handle_call({read, _, _Height, B}, _From, X) ->
    BH = block:prev_hash(0, B),
    P = case dict:find(BH, X#d.dict) of
            error -> error;
            {ok, N} -> 
                {Loc, Size} = dict:fetch(N, X#d.pages),
                read_page(Loc, Size, X#d.blocks_hd)
        end,
    {reply, P, X};
handle_call(_, _From, X) -> {reply, X, X}.

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
    zlib:deflateInit(S, 9),
    B1 = zlib:deflate(S, term_to_binary(X)),
    B2 = zlib:deflate(S, <<>>, finish),
    zlib:close(S),
    %zlib:compress(term_to_binary(X)).
    list_to_binary([B1, B2]).
uncompress(X) ->
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
    B = dict:fetch(Head, D),
    BH = block:prev_hash(0, B),
    old_blocks(BH, ForkTolerance - 1, D).
old_blocks2(Blocks, Size, Head, D) ->
    case dict:find(Head, D) of
        error -> {Blocks, Size};
        {ok, Block} ->
            if
                is_integer(Block) -> {Blocks, Size};
                true ->
                    old_blocks2(dict:store(Head, Block, Blocks),
                                Size + size(term_to_binary(Block)),
                                block:prev_hash(0, Block),
                                D)
            end
    end.
    
    
check_compress(X) ->
    {ok, FT} = application:get_env(amoveo_core, fork_tolerance),
    %FT = 2,
    RB = X#d.ram_bytes,
    if
        ((RB > ?ram_limit) and 
         (X#d.many_blocks > (FT * 2)))->
            TH = block:hash(headers:top_with_block()),
            {Blocks, Bytes} = old_blocks(TH, FT, X#d.dict),%gather up a dict of all the blocks we are moving to the hd.
            {Loc, Size} = write_page(Blocks, X),
            PageNumber = X#d.page_number,
            Pages = dict:store(PageNumber, {Loc, Size}, X#d.pages),
            BK = dict:fetch_keys(Blocks),
            Dict2 = replace_block_with_page(BK, PageNumber, X#d.dict), %replace many blocks in dict with their page number.
            %delete any old uncles from dict, and reduce ram_bytes accordingly.
            {ManyUncles, Dict3} = remove_uncles_before(block:height() - FT, dict:fetch_keys(Dict2), Dict2, 0),
            %io:fwrite([X#d.many_blocks, BK, ManyUncles]),
            X#d{dict = Dict3, 
                pages = Pages, 
                page_number = PageNumber + 1, 
                ram_bytes = RB - Bytes,
                many_blocks = X#d.many_blocks - length(BK) - ManyUncles,
                ram_height = block:height() - FT,
                hd_bytes = X#d.hd_bytes + Size};
        true -> X
    end.
replace_block_with_page([], _, D) -> D;
replace_block_with_page([H|T], N, D) ->
    D2 = dict:store(H, N, D),
    replace_block_with_page(T, N, D2).
remove_uncles_before(_, [], D, U) -> {U, D};
remove_uncles_before(Height, [H|T], D, U) ->
    B = dict:fetch(H, D),
    if
        is_integer(B) -> remove_uncles_before(Height, T, D, U);
        true ->
            BH = element(2, B),
            {U2, D2} = if
                           Height > BH -> {U+1, dict:erase(H, D)};
                           true -> {U, D}
                       end,
            remove_uncles_before(Height, T, D2, U2)
    end.
read_dict2(0, _, _, _) -> [];
read_dict2(N, BH, D, RH) ->
    case dict:find(BH, D) of
        error -> [];
        {ok, B} ->
            Height = element(2, B),
            case Height of
                0 -> [B];
                RH -> [B];
                _ ->
                    BH2 = block:prev_hash(0, B),
                    [B|read_dict2(N-1, BH2, D, RH)]
            end
    end.
            

test() ->
    NewDict = dict:new(),
    {ok, F} = file:open(?blocks_loc, [write, read, raw, binary]),
    X = #d{blocks_hd = F},
    {Loc, Size} = write_page(NewDict, X),
    {ok, Data} = read_page(Loc, Size, X#d.blocks_hd),
    uncompress(Data).
    
















read(Many, Height) ->
    case ?version of
        old ->
            H = block:height(),
            X = min(H, Height + Many),
            M = max(1, 1 + X - Height),
            B = block:get_by_height(X),
            BH = block:prev_hash(0, B),
            lists:reverse([B|read2(M, BH)]);
        new ->
            %io:fwrite("block db read/2 "),
            %io:fwrite(packer:pack([Many, Height])),
            %io:fwrite("\n"),
            H = block:height(),
            X = min(H, Height + Many),
            Block = block:get_by_height(X),
            {ok, Z} = gen_server:call(?MODULE, {read, Many, Height, Block}),
            Z
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
    case ?version of
        old ->
            BlockFile = binary_to_file_path(blocks, Hash),
            case db:read(BlockFile) of
                [] -> empty;
                Block -> uncompress(Block)
            end;
        new ->
            gen_server:call(?MODULE, {read, Hash})
    end.

write(Block, Hash) ->
    case ?version of
        old ->
            CompressedBlockPlus = compress(Block),
                                                %Hash = block:hash(Block),
            BlockFile = binary_to_file_path(blocks, Hash),
            ok = db:save(BlockFile, CompressedBlockPlus);
        new ->
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



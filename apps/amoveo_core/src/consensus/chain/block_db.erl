-module(block_db).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/1, read/2, write/2,
         read_reverse/2,
         read_by_height/1,
         load_page/1,
         uncompress/1, compress/1,
         check/0, by_height_from_compressed/2,
         ram_height/0, genesis/0, exists/1,
         set_ram_height/1,
         test/0]).
-include("../../records.hrl").
-define(LOC, constants:block_db_dict()).
-define(LOC2, constants:block_db_dict2()).
-define(blocks_loc, constants:blocks_file2()).
-define(version, 2).%1 stores the recent blocks in a dictionary, 2 stores them in ets.
%-define(ram_limit, 20000000).%20 megabytes
%-define(ram_limit, 200000).%200 kilobytes
%-define(ram_limit, 10000).
-record(d, {dict = dict:new(), %blocks in ram
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
handle_cast({write_empty, Block, Hash}, X) -> 
    %this seems unused.
    D2 = store(Hash, Block, X#d.dict),
    X2 = X#d{dict = D2, ram_bytes = X#d.ram_bytes, many_blocks = X#d.many_blocks},
    X4 = case element(2, Block) of
             0 -> X2#d{genesis = Block};
             _ -> X2
         end,
    {noreply, X4};
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
handle_cast({load_page, U, Start, End}, X) -> 
    %{Start, End} = height_range(U),
%    io:fwrite("block db load page "),
%    io:fwrite(integer_to_list(Start)),
%    io:fwrite(" "),
%    io:fwrite(integer_to_list(End)),
%    io:fwrite("\n"),
    {Loc, Size} = write_page(U, X),
    PageNumber = X#d.page_number,
    Pages2 = dict:store(PageNumber, {Loc, Size, Start, End}, X#d.pages),
    BK = dict:fetch_keys(U),
    Dict2 = replace_block_with_page(BK, PageNumber, X#d.dict),
    X2 = X#d{dict = Dict2,
             page_number = PageNumber + 1,
             pages = Pages2,
             hd_bytes = X#d.hd_bytes + Size},
    {noreply, X2};
handle_cast({set_ram_height, N}, X) -> 
    X2 = X#d{ram_height = N},
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call(genesis, _From, X) -> 
    {reply, X#d.genesis, X};
handle_call(ram_height, _From, X) -> 
    {reply, X#d.ram_height, X};
handle_call({exists, Hash}, _From, X) -> 
    D = X#d.dict,
    R = case lookup(Hash, D) of
            error -> false;
            _ -> true
        end,
    {reply, R, X};
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
handle_call({read, 0, Height, _B}, _From, X) 
  when (Height >= X#d.ram_height) -> 
    {reply, {ok, []}, X};
handle_call({read, Many0, Height, B}, _From, X) 
  when (Height >= X#d.ram_height) -> 
    BHeight = B#block.height,
    H = block:height(),
    Many = min(Many0 - 1, H - Height),
    M = min(Many, BHeight),
    BH = block:prev_hash(0, B),
    RH = X#d.ram_height,
    Y = lists:reverse([B|read_dict2(M, BH, X, RH)]),

    %H = block:height(),
    %H2 = min(H, Height + Many),
    %M = max(1, 1 + H2 - Height),
    %BH = block:prev_hash(0, B),
    %RH = X#d.ram_height,
    %Y = lists:reverse([B|read_dict2(M, BH, X, RH)]),
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
    case file:pread(File, Loc, Size) of
        eof ->
            timer:sleep(50),
            read_page(Loc, Size, File);
        X -> X
    end.
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
uncompress(X) when is_binary(X) ->
    S = zlib:open(),
    zlib:inflateInit(S),
    A = zlib:inflate(S, X),
    B = list_to_binary(A),
    Y = binary_to_term(B),
    %Y = binary_to_term(list_to_binary(zlib:inflate(S, X))),
    zlib:close(S),
    Y;
uncompress(X) when is_list(X) -> 
    %it is a list of blocks, but we want to return it in a page format.
    blocks_to_page_safe(X).

    %binary_to_term(zlib:uncompress(X)).

blocks_to_page_safe(L) ->
    lists:foldl(
      fun(X, Acc) -> 
              dict:store(block:hash(X), X, Acc) end, 
      dict:new(), L).
    

blocks_to_page(L) ->    
    L2 = lists:reverse(L),
    blocks_to_page2(L2, block:hash(hd(L2)), dict:new()).
blocks_to_page2([Block], Hash, D) ->
    dict:store(Hash, Block, D);
blocks_to_page2([Block|T], Hash, D) ->
    blocks_to_page2(T, Block#block.prev_hash,
                    dict:store(Hash, Block, D)).
            

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
                    %Block2 = Block,
                    old_blocks2(dict:store(Head, Block2, Blocks),
                                Size + size(term_to_binary(Block2)),
                                block:prev_hash(0, Block),
                                D)
            end
    end.
height_range(Blocks) ->%page of blocks
    K = dict:fetch_keys(Blocks),
    case K of
        [] -> error;
        _ ->
            hr2(K, Blocks, block:height(), 0)
    end.
hr2([], _, Start, End) -> {Start, End};
hr2([H|T], Blocks, S, E) ->
    %B = fetch(H, Blocks),
    B = dict:fetch(H, Blocks),
    Height = B#block.height,
    hr2(T, Blocks, min(S, Height), max(E, Height)).
    
check_compress(X) ->
    {ok, FT0} = application:get_env(amoveo_core, fork_tolerance),
    FT = FT0 * 2,
    {ok, RL} = application:get_env(amoveo_core, block_cache),
    RB = X#d.ram_bytes,
    if
        ((RB > RL) and 
         (X#d.many_blocks > (FT0 * 3)))->
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
    













by_height_from_compressed(error, _) -> error;
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
        Height =< RH ->
            %true = Height < RH,
            true = Height > -1,
            gen_server:call(?MODULE, {read_by_height, Height});
        true -> block:get_by_height(Height)
    end.
load_page(Decompressed) ->
    case height_range(Decompressed) of
        error -> ok;
        {Start, End} ->
            if
                (Start == 275601) ->
                    B275600 = binary_to_term(
<<131,104,21,100,0,5,98,108,111,99,107,98,0,4,52,144,109,
  0,0,0,32,70,247,163,116,7,6,70,65,132,61,159,26,169,58,
  160,52,52,116,136,59,254,207,117,10,123,190,245,111,123,
  129,130,191,109,0,0,0,32,61,218,246,253,111,182,215,110,
  254,83,71,96,183,195,245,217,200,44,77,241,149,164,178,
  43,135,91,21,116,119,182,248,9,98,103,160,170,235,98,0,
  0,47,215,98,0,0,23,94,97,3,110,23,0,164,72,0,0,89,147,0,
  0,0,0,0,33,137,214,57,0,0,0,0,239,232,10,134,98,0,0,90,
  76,108,0,0,0,1,104,4,100,0,8,99,111,105,110,98,97,115,
  101,109,0,0,0,65,4,8,2,224,156,190,226,97,20,54,112,205,
  39,155,105,220,13,233,103,126,185,25,122,98,224,98,36,
  173,187,25,195,75,44,43,220,114,122,177,240,216,11,166,
  135,43,56,189,148,88,212,12,77,90,53,62,235,107,127,202,
  60,61,85,236,217,9,243,97,0,97,0,106,104,10,100,0,11,
  112,114,101,118,95,104,97,115,104,101,115,109,0,0,0,32,
  70,247,163,116,7,6,70,65,132,61,159,26,169,58,160,52,52,
  116,136,59,254,207,117,10,123,190,245,111,123,129,130,
  191,109,0,0,0,32,32,243,178,15,237,23,253,250,58,58,170,
  190,85,76,196,147,174,152,114,12,230,139,23,89,221,48,
  24,115,152,21,103,42,109,0,0,0,32,126,47,49,15,139,198,
  202,209,185,235,90,174,219,244,106,89,166,30,113,29,207,
  64,237,74,141,150,58,218,24,195,10,217,109,0,0,0,32,168,
  156,135,121,207,204,216,48,36,48,115,134,232,50,251,6,
  90,153,147,18,222,167,104,237,169,212,107,39,224,201,
  223,82,109,0,0,0,32,222,39,0,79,71,211,191,10,167,7,29,
  13,130,219,45,197,15,154,129,48,14,34,13,50,75,126,250,
  134,136,191,100,105,109,0,0,0,32,38,215,106,50,198,242,
  155,155,142,149,43,150,57,149,116,179,146,137,30,120,98,
  99,103,22,112,17,149,146,216,4,23,192,109,0,0,0,32,76,
  25,237,217,179,242,229,220,121,166,150,26,188,75,31,176,
  6,201,36,207,24,214,118,76,156,150,223,134,202,69,48,4,
  109,0,0,0,32,69,131,115,136,222,255,65,231,70,112,244,
  196,199,118,61,165,98,72,31,232,130,69,83,79,61,95,108,
  21,236,125,19,110,109,0,0,0,32,18,191,132,210,74,75,28,
  128,145,1,161,25,209,250,231,55,19,91,31,9,18,245,50,60,
  106,163,92,178,217,54,120,200,104,2,109,0,0,3,43,200,
  138,236,105,181,247,138,18,10,208,60,203,5,178,169,113,
  196,16,90,238,51,126,154,211,245,113,109,179,166,47,156,
  4,138,162,37,194,48,171,164,116,173,16,53,159,66,237,62,
  117,214,138,90,244,61,152,16,139,183,44,222,130,105,239,
  195,241,52,41,163,32,217,217,220,255,91,38,88,134,135,
  140,17,213,189,107,223,81,215,186,155,213,106,8,65,5,
  123,229,147,5,154,7,9,254,189,75,145,218,121,198,22,110,
  49,19,107,168,128,3,163,19,91,209,76,34,217,10,195,149,
  95,201,253,11,133,214,144,100,44,133,226,76,131,48,253,
  175,131,153,119,56,123,119,247,80,224,1,161,165,90,56,
  54,126,60,103,213,12,173,128,60,128,208,206,15,187,248,
  151,96,119,104,132,174,171,225,126,70,142,251,184,92,10,
  49,226,223,177,198,64,140,203,235,66,28,232,143,242,203,
  78,91,134,56,236,64,144,225,254,124,223,161,31,176,139,
  15,116,60,147,211,78,36,52,94,165,236,125,43,149,117,69,
  209,158,160,181,190,193,154,62,139,43,51,192,235,80,123,
  192,125,253,0,224,21,252,223,154,214,179,206,134,93,55,
  149,82,189,120,51,67,134,34,57,250,134,196,235,253,175,
  65,154,165,161,204,189,98,102,98,194,194,246,205,169,39,
  79,169,214,67,73,232,139,158,108,194,47,150,239,66,196,
  207,143,15,88,198,125,107,228,182,45,138,169,228,16,180,
  52,191,251,253,63,174,97,142,75,54,170,245,229,212,93,
  133,234,170,226,8,104,12,212,158,223,193,115,217,111,94,
  238,120,206,120,252,87,51,116,151,107,89,185,75,115,181,
  41,236,216,194,218,162,232,10,88,111,92,92,234,40,235,
  91,170,204,109,113,214,215,53,155,80,242,175,233,213,43,
  8,57,109,153,64,156,125,148,106,104,226,248,154,237,103,
  42,223,254,241,57,51,3,219,65,28,36,191,237,126,203,111,
  12,116,152,131,68,191,206,24,31,101,96,1,173,5,143,44,
  218,139,21,208,147,204,58,88,252,187,1,222,77,208,31,
  103,11,185,55,235,222,70,24,218,102,163,99,233,201,212,
  48,70,169,62,6,243,95,157,29,179,160,120,232,247,61,175,
  109,219,147,32,249,82,233,131,132,14,111,61,183,154,43,
  60,140,213,43,88,7,73,8,26,40,85,21,48,58,250,236,248,
  123,165,95,15,181,67,52,65,5,243,67,37,107,191,150,204,
  0,138,211,187,101,77,35,231,105,47,99,192,4,214,218,162,
  174,128,230,33,227,234,93,139,224,108,216,247,169,216,
  101,145,157,2,17,228,234,43,232,12,106,0,217,16,124,226,
  191,135,167,10,45,48,146,166,6,13,159,165,44,175,0,14,
  176,47,107,143,31,73,229,1,70,196,196,251,213,178,234,
  145,244,101,244,170,201,107,30,95,50,21,53,96,188,206,
  211,27,131,172,85,10,86,213,127,60,9,79,141,34,168,130,
  42,25,89,78,33,4,86,117,128,111,132,239,12,55,20,81,132,
  119,8,21,82,245,151,79,124,167,30,45,145,113,12,171,127,
  55,25,41,119,210,203,56,228,97,100,221,240,29,163,151,
  245,184,241,149,154,56,60,30,127,15,65,58,135,212,205,
  118,159,95,26,104,121,72,73,239,240,235,69,60,84,5,33,
  102,71,4,176,58,73,121,61,98,248,84,146,236,1,1,14,171,
  77,51,227,216,96,180,180,196,102,50,235,110,66,207,127,
  39,220,252,10,171,168,208,119,133,143,79,178,22,133,103,
  48,0,4,235,1,222,233,213,116,190,93,101,28,155,124,97,
  172,171,42,122,216,90,1,171,93,26,240,78,75,171,12,174,
  30,202,65,78,22,17,0,4,155,108,0,0,0,2,104,6,100,0,3,97,
  99,99,110,6,0,75,188,207,80,104,1,97,1,109,0,0,0,65,4,
  189,18,206,25,5,24,85,181,145,52,221,156,239,44,26,124,
  15,19,53,47,199,101,54,159,33,2,193,105,148,36,244,97,
  47,22,207,60,175,158,167,199,152,51,25,83,197,83,191,
  194,116,18,229,105,172,24,130,156,172,243,251,252,92,53,
  89,87,97,1,109,0,0,0,0,104,6,100,0,3,97,99,99,110,5,0,
  202,205,98,20,1,98,0,0,7,5,109,0,0,0,65,4,8,2,224,156,
  190,226,97,20,54,112,205,39,155,105,220,13,233,103,126,
  185,25,122,98,224,98,36,173,187,25,195,75,44,43,220,114,
  122,177,240,216,11,166,135,43,56,189,148,88,212,12,77,
  90,53,62,235,107,127,202,60,61,85,236,217,9,243,97,1,
  109,0,0,0,0,106,109,0,0,0,32,65,49,38,148,183,188,205,
  45,28,52,111,228,192,176,88,99,197,80,148,83,246,218,
  254,18,235,97,156,184,107,224,91,5,109,0,0,0,0,110,6,0,
  223,79,11,227,97,8,110,5,1,96,139,220,93,2,97,12,98,0,0,
  20,133,98,0,0,1,125,97,205>>),
                    BH = block:hash(B275600),
                    D2 = dict:store(BH, B275600, Decompressed),
                    gen_server:cast(?MODULE, 
                                    {load_page, 
                                     D2, Start-1, End});
                true ->
                    gen_server:cast(?MODULE, 
                                    {load_page, 
                                     Decompressed, Start, End})
            end
    end.
set_ram_height(N) ->
    gen_server:cast(?MODULE, {set_ram_height, N}).
ram_height()  ->
    gen_server:call(?MODULE, ram_height).
genesis()  ->
    gen_server:call(?MODULE, genesis).
    
exists(Hash) -> 
    gen_server:call(?MODULE, {exists, Hash}).
read_reverse(Many, Highest) ->
    %highest is the highest in the range of blocks that we want to return.
    % if we return a page, make sure highest is included.
    H = block:height(),
    RH = ram_height(),
    if
        %(Highest =< RH) -> 
        (Highest < RH) -> 
            read_by_height(Highest);
        true ->
            StartHeight = max(Highest - Many, RH),
            Block = block:get_by_height(Highest),
            {ok, Z} = gen_server:call(?MODULE, {read, Highest-StartHeight, StartHeight, Block}),
            Z
    end.
            
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
                    X = min(H, Height + Many - 1),
                    Block = block:get_by_height(X),
                    D = max(0, Height - H),
                    %Height - D
                    %height - max(0, height - h)
                    %if height > h -> h
                    %if height < h -> height
                    HD = min(Height, H),

                    %{ok, Z} = gen_server:call(?MODULE, {read, Many-D, Height-D, Block}),
                    {ok, Z} = gen_server:call(?MODULE, {read, Many-D, HD, Block}),
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
    Height = Block#block.height,
    Block2 = case Height of
                 0 -> Block;
                 _ ->
                    {ok, PrevHeader} = headers:read(Block#block.prev_hash),
                     PrevHashes = block:calculate_prev_hashes(PrevHeader),
                     Block#block{prev_hashes = PrevHashes}
                     %Block
             end,
    case Version of
        1 ->
            CompressedBlockPlus = compress(Block2),
                                                %Hash = block:hash(Block),
            BlockFile = binary_to_file_path(blocks, Hash),
            ok = db:save(BlockFile, CompressedBlockPlus);
        _ ->
                gen_server:cast(?MODULE, {write, Block2, Hash})
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

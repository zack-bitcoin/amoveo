%in this version of the block_db, we just put every block on the hard drive. in ram we remember a connection between the hash of each block, and where it is stored on the hard drive. And, we remember a connection between every integer 0-(current block height) and the blockhash that is in the longest chain for that height.

-module(block_db3).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
read/1, read/2, read_compressed/2, write/1, write/2, set_top/1, genesis/0, test/1, check/0, make_zlib_dictionary/0, get_pid/0, zlib_dictionary/0, zlib_reload/1,
compress/1, uncompress/1, compress/2, uncompress/2,
compress2/1, 
update_pointer/2, exists/1, rewrite/1,
copy_everything_from_block_db/1]).
-include("../../records.hrl").
-define(LOC, constants:block_db3_dict()). %this file stores the #d record. The ram part of this gen server.
-define(blocks_loc, constants:blocks_file4()). %this is where we store  blocks.
%test
% size(term_to_binary(block_db3:compress(block_db3:read(349990, 350000)))).
-define(lzip_file, "../../../../lzip_dictionary"). 

-record(d, {hash2block = dict:new(),%for looking up blocks using blockhashes.
            height2hash = dict:new(),%for looking up block hashes using block heights.
            file_pointer = 0,%pointer to first empty byte in the file.
            top = <<>>,
            zlib_dictionary,
            file}).%where we store blocks.
init(ok) -> 
    io:fwrite("block_db3 init\n"),
    process_flag(trap_exit, true),
    {ok, F} = file:open(?blocks_loc, [write, read, raw, binary]),
    X = db:read(?LOC),
    %ZLIB = element(2, file:read_file("../../../../lzip_dictionary")),
    ZLIB = element(2, file:read_file(?lzip_file)),
    Ka = if
             X == "" ->
                 G = block:genesis_maker(),
                 GH = block:hash(G),
                 CG = compress(G, ZLIB),
                 {H2B, P} = raw_write(CG, GH, 1, F, dict:new()),                 
                 %{H2B, P} = internal_write(G, GH, 1, F, dict:new()),
                 H2H = dict:store(GH, 1, dict:new()),
                 #d{hash2block = H2B, height2hash = H2H,
                    zlib_dictionary = ZLIB,
                    file_pointer = P+1};
             true -> X
         end,
    K2 = Ka#d{file = F},
    io:fwrite("block_db3 init done\n"),
    {ok, K2}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    file:close(X#d.file),
    X2 = X#d{file = 0},
    db:save(?LOC, X2),
    io:format("block_db3 died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({set_top, Hash}, X) -> 
    #d{
       height2hash = H2H,
       hash2block = H2B,
       file = File,
       zlib_dictionary = ZD,
       top = _
      } = X,
    H2H2 = set_top_loop(Hash, H2B, H2H, File, ZD),
    X2 = X#d{height2hash = H2H2},
    {noreply, X2};
handle_cast({rewrite, Blocks}, X) -> 
    %todo. reduce the file pointer to before the blocks that we are rewriting.
    [{Hash1, _}|_] = Blocks,
    case dict:find(Hash1, X#d.hash2block) of
        {ok, {P, _}} ->
            X2 = internal_rewrite_loop(Blocks, X#d{file_pointer = P}),
            {noreply, X2};
        error ->
            io:fwrite("block_db3 rewrite impossible error"),
            {noreply, X}
    end;
handle_cast({write, Block, Hash}, X) -> 
    P = X#d.file_pointer,
    File = X#d.file,
    H2B = X#d.hash2block,
    ZD = X#d.zlib_dictionary,
    case dict:find(Hash, H2B) of
        error ->
            {H2B2, Size} = internal_write(Block, Hash, P, File, H2B, ZD),
            X2 = X#d{hash2block = H2B2, file_pointer = P + Size},
            {noreply, X2};
        {ok, _} -> {noreply, X}
    end;
handle_cast({zlib_reload, Binary}, X) -> 
    X2 = X#d{
           zlib_dictionary = Binary
          },
    {noreply, X2};
%handle_cast({update_pointer, Hash, Pointer}, X) -> 
%    #d{hash2block = H2B, file = File, zlib_dictionary = ZD} = X,
%    case dict:find(Hash, H2B) of
%        error -> {noreply, X};
%        {ok, {OldLoc, Size1}} ->
%            Block = read_from_file(OldLoc, Size1, File, ZD, uncompressed),
%            Block2 = Block#block{trees = Pointer},
%            io:fwrite("change points " ++ Pointer, Block#block.trees),
%            {H2B2, Size2} = internal_write(Block2, Hash, OldLoc, File, H2B, ZD),
%            false = Size2 > Size1,
%            X2 = X#d{hash2block = H2B2},
%            {noreply, X2}
%    end;
handle_cast(_, X) -> {noreply, X}.
handle_call(zlib, _From, X) -> 
    #d{
       zlib_dictionary = ZD
      } = X,
    {reply, ZD, X};
handle_call({read, Hash}, _From, X) -> 
    #d{
       hash2block = H2B,
       file = File, 
       zlib_dictionary = ZD
      } = X,
    R = read_hash(Hash, H2B, File, ZD, uncompressed),
    {reply, R, X};
handle_call({read, Start, End, Type}, _From, X) -> 
    #d{
       height2hash = H2H,
       hash2block = H2B,
       zlib_dictionary = ZD,
       file = File
      } = X,
    R = read_loop(Start, End, H2H, H2B, File, ZD, Type),
    {reply, R, X};
handle_call(get_pid, _From, X) -> {reply, self(), X};
handle_call(top, _From, X) -> {reply, X#d.top, X};
handle_call(_, _From, X) -> {reply, X, X}.

internal_rewrite_loop([], X) -> X;
internal_rewrite_loop([{Hash, Block}|Blocks], X) -> 
    P = X#d.file_pointer,
    File = X#d.file,
    H2B = X#d.hash2block,
    ZD = X#d.zlib_dictionary,
    {H2B2, Size} = internal_write(Block, Hash, P, File, H2B, ZD),
    X2 = X#d{hash2block = H2B2, file_pointer = P + Size},
    internal_rewrite_loop(Blocks, X2).
internal_write(Block0, Hash, P, File, H2B, ZD) ->
    %Block = compress(Block0),
    Block = compress(Block0, ZD),
    raw_write(Block, Hash, P, File, H2B).
raw_write(Block, Hash, P, File, H2B) ->
    Size = size(Block),
    file:pwrite(File, P, Block),
    H2B2 = dict:store(Hash, {P, Size}, H2B),
    {H2B2, Size}.
read_from_file(Loc, Size, File, ZD, Compressed) ->
    case file:pread(File, Loc, Size) of
        eof ->
            timer:sleep(50),
            read_from_file(Loc, Size, File, ZD, Compressed);
        {ok, X} -> 
            case Compressed of
                compressed -> X;
                uncompressed ->
                    uncompress(X, ZD)
            end
    end.
read_loop(Start, End, _, _, _, _, _) when (Start > End) -> [];
read_loop(Start, End, H2H, H2B, File, ZD, Compressed) ->
    case dict:find(Start, H2H) of
        error -> [];
        {ok, Hash} ->
            [read_hash(Hash, H2B, File, ZD, Compressed)|
             read_loop(Start+1, End, H2H, H2B, File, ZD, Compressed)]
    end.
set_top_loop(Hash, H2B, H2H, File, ZD) ->
    case dict:find(Hash, H2B) of
        error -> H2H;
        {ok, {P, Size}} ->
            Block = read_from_file(P, Size, File, ZD, uncompressed),
            PrevHash = Block#block.prev_hash,
            Height = Block#block.height,
            case dict:find(Height, H2H) of
                {ok, Hash} -> H2H;
                _ ->
                    H2H2 = dict:store(Height, Hash, H2H),
                    set_top_loop(PrevHash, H2B, H2H2, File, ZD)
            end
    end.
            
read_hash(Hash, H2B, File, ZD, Compressed) ->
    case dict:find(Hash, H2B) of
        error -> <<>>;
        {ok, {Location, Size}} ->
            read_from_file(Location, Size, File, ZD, Compressed)
    end.

%"BKdkHXUeBIgzqyQ0morfNcw2AKIc/n1NAt0pK34ESnaC62mpSSMAqMsArWIqcyWWACdIL9r82UhnuUJIbueRH04=" old mining address
%"BC80oG/EAXojuLCjIjQmIQgTv9wHscgCccEy4q7R2Vwak2iPbrTb1htgVOU+NjChxxmOeNiUJMxURPqUEWZ2lzc=" old mining address
%<<4,8,2,224,156,190,226,97,20,54,112,205,39,155,105,220,13,233,103,126,185,25,122,98,224,98,36,173,187,25,195,75,44,43,220,114,122,177,240,216,11,166,135,43,56,189,148,88,212,12,77,90,53,62,235,107,127,202,60,61,85,236,217,9,243>>, % mining pool 1
%<<4,189,18,206,25,5,24,85,181,145,52,221,156,239,44,26,124,15,19,53,47,199,101,54,159,33,2,193,105,148,36,244,97,47,22,207,60,175,158,167,199,152,51,25,83,197,83,191,194,116,18,229,105,172,24,130,156,172,243,251,252,92,53,89,87>> % dev reward address
%"BJJxz7fHEmpuM5zo8I4EPeG6lY9DKXsf8/SwpVwW9JyiXBaTwS0gekjrNQ/lYuUGVlqlqqPFLIfD0rNUSA6g6pU="
%"BEB/9uVjQrLVkXtBKHCmY+/+Pn3ih8IIwfzdex0wYqRkcRbdiJSkTqfI+YiNzyjgSKCGOPR+yhLg4zlfiFoWNjo="
%"BAgC4Jy+4mEUNnDNJ5tp3A3pZ365GXpi4GIkrbsZw0ssK9xyerHw2Aumhys4vZRY1AxNWjU+62t/yjw9VezZCfM="
%"BEyA9KTOfkBPe8I0QgdD3iGic5ELz92hcRxs+KdswbH4yLsZz5bhlS1Cwzngx6N9RGGGP907qmMC8laVRdSLveQ="




%<<4,167,100,29,117,30,4,136,51,171,36,52,154,138,223,53,204,54,0,162,28,254,125,77,2,221,41,43,126,4,74,118,130,235,105,169,73,35,0,168,203,0,173,98,42,115,37,150,0,39,72,47,218,252,217,72,103,185,66,72,110,231,145,31,78>>,
%<<4,47,52,160,111,196,1,122,35,184,176,163,34,52,38,33,8,19,191,220,7,177,200,2,113,193,50,226,174,209,217,92,26,147,104,143,110,180,219,214,27,96,84,229,62,54,48,161,199,25,142,120,216,148,36,204,84,68,250,148,17,102,118,151,55>>,


%-define(zlibDict, element(2, file:read_file("../../../../lzip_dictionary"))). 
compress([]) -> [];
compress([A|B]) -> 
    [compress(A)|compress(B)];
compress(Uncompressed0) -> 
    ZD = zlib_dictionary(),
    compress(Uncompressed0, ZD).
compress(Uncompressed0, ZD) ->
    %Uncompressed = term_to_binary(Uncompressed0#block{prev_hashes = <<>>}),
    Uncompressed = term_to_binary(Uncompressed0),
    Z = zlib:open(),
    ok = zlib:deflateInit(Z, 8, deflated, 15, 8, default),
    %DID = zlib:deflateSetDictionary(Z, ?zlibDict),
    DID = zlib:deflateSetDictionary(Z, ZD),
    Compressed1 = zlib:deflate(Z, Uncompressed),
    Compressed2 = zlib:deflate(Z, [], finish),
    Compressed = list_to_binary([Compressed1|Compressed2]),
    ok = zlib:deflateEnd(Z),
    Compressed.
uncompress(Compressed) ->
    ZD = zlib_dictionary(),
    uncompress(Compressed, ZD).
uncompress(Compressed, ZD) ->
    Z = zlib:open(),
    zlib:inflateInit(Z),
    case zlib:inflate(Z, Compressed, [{exception_on_need_dict, false}]) of
        {need_dictionary, DID, []} -> 
            %ok = zlib:inflateSetDictionary(Z, ?zlibDict),
            ok = zlib:inflateSetDictionary(Z, ZD),
            A = zlib:inflate(Z, Compressed),
            Uncompressed0 = A,
    %D = zlib:inflateGetDictionary(Z),
            ok = zlib:inflateEnd(Z),
            binary_to_term(list_to_binary(Uncompressed0));
        [X] ->
            zlib:inflateEnd(Z),
            binary_to_term(X);
        Y ->
            zlib:inflateEnd(Z),
            binary_to_term(list_to_binary(Y))
    end.
compress2(X) ->
    %term_to_binary(X).
    %block_db:compress(X).
    %zlib:compress(term_to_binary(X)).

    S = zlib:open(),
    {ok, CL} = application:get_env(amoveo_core, compression_level),
    zlib:deflateInit(S, CL),
    B1 = zlib:deflate(S, term_to_binary(X)),
    B2 = zlib:deflate(S, <<>>, finish),
    zlib:close(S),
    list_to_binary([B1, B2]).
uncompress2(X) ->
    %binary_to_term(X).
    %block_db:uncompress(X).
    %Y = binary_to_term(list_to_binary(zlib:inflate(S, X))),
    S = zlib:open(),
    zlib:inflateInit(S),
    A = zlib:inflate(S, X),
    B = list_to_binary(A),
    Y = binary_to_term(B),
    zlib:close(S),
    Y.

check() ->
    io:fwrite("block db3 check\n"),
    gen_server:call(?MODULE, check).
exists(Hash) ->
    not(read(Hash) == <<>>).
read(Hash) ->
    io:fwrite("block db3 read\n"),
    gen_server:call(?MODULE, {read, Hash}).
read(Start, End) ->
    io:fwrite("block db3 read2\n"),
    gen_server:call(?MODULE, {read, Start, End, uncompressed}).
read_compressed(Start, End) ->
    io:fwrite("block db3 read compressed\n"),
    gen_server:call(?MODULE, {read, Start, End, compressed}).
zlib_dictionary() ->
    io:fwrite("block db3 zlib dic\n"),
    gen_server:call(?MODULE, zlib).
zlib_reload(Bin) ->
    io:fwrite("block db3 zlib reload\n"),
    gen_server:cast(?MODULE, {zlib_reload, Bin}).
update_pointer(Hash, Pointer) ->
    io:fwrite("block db3 update pointer\n"),
    gen_server:cast(?MODULE, {update_pointer, Hash, Pointer}).
write(Block) ->
    io:fwrite("block db3 write\n"),
    Hash = block:hash(Block),
    write(Block, Hash),
    Hash.
write(Block, Hash) ->
    io:fwrite("block db3 write 2\n"),
    Bool = (is_integer(Block#block.trees)),
    Bool2 = is_record(Block#block.trees, trees),
    Bool3 = is_record(Block#block.trees, trees5),
    if
        Bool -> %io:fwrite("cant store block 1\n"),
                ok;
        Bool2 -> %io:fwrite("can't store block 2\n"),
                 ok;
        Bool3 -> %io:fwrite("can't store block 3\n"),
                 ok;
        true -> %io:fwrite(Block#block.trees)
            ok
    end,
    H = headers:top_with_block(),
    Hash2 = block:hash(H),
    gen_server:cast(?MODULE, {write, Block, Hash}),
    recent_blocks:add(Hash, Block#block.height),
    %if this is the top of the headers, then do a set_top(Hash).
    spawn(fun() ->
                  if
                      (Hash2 == Hash) ->
                          set_top(Hash),
                          tx_pool_feeder:absorb_dump2(Block, []),
                          %tx_reserver:restore(),
                          potential_block:new();
                      true -> ok
                  end
          end).
rewrite(Blocks) ->
    io:fwrite("block db3 rewrite\n"),
    gen_server:cast(?MODULE, {rewrite, Blocks}).%[{Hash1, Block1},...]
    
set_top(Hash) ->
    io:fwrite("block db3 set top\n"),
    gen_server:cast(?MODULE, {set_top, Hash}).
genesis() ->
    io:fwrite("block db3 genesis\n"),
    read(1, 1).
get_pid() ->
    io:fwrite("block db3 get pid\n"),
    gen_server:call(?MODULE, get_pid).
top() ->
    io:fwrite("block db3 top\n"),
    gen_server:call(?MODULE, top).

test(1) ->
    copy_everything_from_block_db(0),
    set_top(block:hash(block:get_by_height(350000))).

max_of_list([X]) -> X;
max_of_list([A|B]) -> 
    max(A, max_of_list(B)).
copy_everything_from_block_db(Height) ->
    io:fwrite("copy_everything_from_block_db. height: "),
    io:fwrite(integer_to_list(Height)),
    io:fwrite(" megabytes used: "),
    Pid = get_pid(),
    io:fwrite(integer_to_list(element(2, hd(process_info(Pid, [memory]))) div 1000000)),
    io:fwrite("\n"),
    BH = block:height(),
    GC = BH rem 1000,
    if
        GC == 0 ->
            %timer:sleep(1000);
            ok;
        true -> ok
    end,
    if
        Height > BH -> ok;
        true ->
            X = block_db:read_by_height(Height),
            case X of
                #block{} -> 
                    Hash = block:hash(X),
                    block_db3:write(X, Hash),
                    copy_everything_from_block_db(Height + 1);
                _ ->
                    Dict = block_db:uncompress(X),
                    Keys = dict:fetch_keys(Dict),
                    Heights = 
                        lists:map(
                          fun(Hash) ->
                                  Block = dict:fetch(Hash, Dict),
                                  block_db3:write(Block, Hash),
                                  Block#block.height
                          end, Keys),
                    NewHeight = max_of_list(Heights)+1,
                    copy_everything_from_block_db(NewHeight)
            end
    end.
make_zlib_dictionary() ->    
    CommonThings = make_zlib_scan_blocks(0, 350000, dict:new()),
    CommonThings.
make_zlib_scan_blocks(Start, End, Dict) when Start > End -> Dict;
make_zlib_scan_blocks(Start, End, Dict) -> 
    R = Start rem 1000,
    if
        (R == 1) ->
            io:fwrite("zlib scan block height "),
            io:fwrite(integer_to_list(Start)),
            io:fwrite(" "),
            io:fwrite(integer_to_list(element(2, hd(process_info(self(), [memory]))) div 1000000)),
            io:fwrite("\n");
        true -> ok
    end,
    Block = hd(read(Start, Start)),
    Block2 = Block#block{height = 0, trees_hash = 0, time = 0, nonce = 0, prev_hashes = 0, proofs = 0, meta = 0},
    Dict2 = occurances(Block2, Dict),
    make_zlib_scan_blocks(Start+1, End, Dict2).
occurances(#signed{data = D}, Dict) ->
    occurances(D, Dict);
occurances(X, Dict) when is_list(X) ->
    lists:foldl(fun(Y, A) -> occurances(Y, A) end, Dict, X);
occurances(X, Dict) when is_tuple(X) ->
    occurances(tuple_to_list(X), Dict);
occurances(X, Dict) ->
    case dict:find(X, Dict) of
        error -> dict:store(X, 1, Dict);
        {ok, N} -> dict:store(X, N+1, Dict)
    end.


                      

            
       

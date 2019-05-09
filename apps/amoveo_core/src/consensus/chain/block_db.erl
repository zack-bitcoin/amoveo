-module(block_db).
-export([get/1, put/1]).

get(Hash) ->
    BlockFile = binary_to_file_path(blocks, Hash),
    case db:read(BlockFile) of
        [] -> empty;
        Block -> binary_to_term(zlib:uncompress(Block))
    end.

put(Block) ->
    CompressedBlockPlus = zlib:compress(term_to_binary(Block)),
    Hash = block:hash(Block),
    BlockFile = binary_to_file_path(blocks, Hash),
    ok = db:save(BlockFile, CompressedBlockPlus).
    

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

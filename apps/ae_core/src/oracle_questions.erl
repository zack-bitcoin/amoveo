-module(oracle_questions).
-export([save/1, read/1, all/0]).
-define(LOC, constants:oracle_questions()).
all() ->
    {Trees, _, _} = tx_pool:data(),
    Oracles = trees:oracles(Trees),
    Oracles2 = trie:get_all(Oracles, oracles),
    all2(Oracles2).
all2([]) -> ok;
all2([Leaf|T]) -> 
    Oracle = leaf:value(Leaf),
    Q = oracles:question(Oracle),
    [Q|all2(T)].

read(Hash) ->
    OracleQuestionsFile = ae_utils:binary_to_file_path(oracle_questions, Hash),
    case db:read(OracleQuestionsFile) of
        [] ->
            empty;
        OracleQuestion ->
            zlib:uncompress(OracleQuestion)
    end.

save(Binary) ->
    CompressedOracleQuestion = zlib:compress(Binary),
    Binary = zlib:uncompress(CompressedOracleQuestion), % Sanity check, not important for long-term
    Hash = block:hash(Binary),
    OracleQuestionFile = ae_utils:binary_to_file_path(oracle_questions, Hash),
    db:save(OracleQuestionFile, CompressedOracleQuestion).

-module(backup).
-export([hash/0, backup/0, backup_files/0, read/2, read_size/1, files/0]).
files() -> [constants:blocks(), constants:block_pointers(), constants:accounts(), constants:all_secrets(), constants:d_accounts(), constants:channels(), constants:d_channels(), constants:entropy()].

backup_files() -> tl(tl(files())).
hash() -> hash(backup_files(), []).
hash([], X) -> testnet_hasher:doit(X);
hash([F|T], X) -> hash(T, [hash:file(F)|X]).
-define(backup, "backup/").
backup() -> backup(backup_files()).
backup([]) -> ok;
backup([F|T]) -> 
    file:copy(F, ?backup++F),
    backup(T).

-define(word, constants:word_size()).
read_size(File) ->
    filelib:file_size(?backup++File) div ?word.
read(File, N) ->
    {ok, RFile } = file:open(?backup++File, [read, binary, raw]),
    {ok, Out} = file:pread(RFile, N*?word, ?word),
    file:close(RFile),
    Out.


-module(csc).%consensus state cache.
-export([new_db/0, is_empty/2, add_empty/3, add/4, update/3, read/2, test/0]).

%While processing txs, there is a slice of the consensus state that these txs will be touching. We store this slice in ram. Parts can get updated while processing the txs. We will use this updated data to calculate the new consensus state root for the block.

%this database has a clear distinction between what we are certain are empty parts of the database, vs parts of the database that are not included in this slice.

-record(r, {empty = true, val, key, unhashed_key}).

new_db() ->
    dict:new().

is_empty(K, DB) ->
    %returns true if we have a cryptographic proof that this part of the consensus state is empty.
    %returns error if it is outside our slice.
    case dict:find(K, DB) of
        error -> error;
        {ok, V = #r{empty = E}} -> E
    end.

add_empty(K, UK, DB) ->
    R = #r{key = K, unhashed_key = UK},
    dict:store(K, R, DB).

add(K, UK, Val, DB) ->
    R = #r{empty = false, val = Val, key = K, unhashed_key = UK},
    dict:store(K, R, DB).

update(K, Val, DB) ->
    {ok, R} = dict:find(K, DB),
    R2 = R#r{empty = false, val = Val},
    dict:store(K, R2, DB).

read(K, DB) ->
    case dict:find(K, DB) of
        error -> error;
        {ok, R} ->
            case R#r.empty of
                true -> {empty, R#r.unhashed_key};
                false -> {ok, R#r.val}
            end
    end.
            
test() ->    
    DB = new_db(),
    error = is_empty(1, DB),
    error = read(1, DB),
    DB2 = add_empty(1, one, DB),
    {empty, one} = read(1, DB2),
    true = is_empty(1, DB2),
    DB3 = add(2, two, "two", DB2),
    false = is_empty(2, DB3),
    true = is_empty(1, DB3),
    DB4 = update(1, "one", DB3),
    false = is_empty(1, DB4),
    {ok, "two"} = read(2, DB4),
    {ok, "one"} = read(1, DB4),
    success.
                 
            

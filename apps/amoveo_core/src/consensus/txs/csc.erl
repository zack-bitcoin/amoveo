-module(csc).%consensus state cache.
-export([new_db/0, is_empty/2, add_empty/4, add/5, update/3, read/2, read2/2, test/0]).

%While processing txs, there is a slice of the consensus state that these txs will be touching. We store this slice in ram. Parts can get updated while processing the txs. We will use this updated data to calculate the new consensus state root for the block.

%this database has a clear distinction between what we are certain are empty parts of the database, vs parts of the database that are not included in this slice.


-include("../../records.hrl").

new_db() ->
    dict:new().

is_empty(UK, DB) ->
    %returns true if we have a cryptographic proof that this part of the consensus state is empty.
    %returns error if it is outside our slice.
    case dict:find(UK, DB) of
        error -> error;
        {ok, V = #consensus_state{empty = E}} -> E
    end.

add_empty(Type, K, UK, DB) ->
    error = dict:find(UK, DB),
    R = #consensus_state{key = K, unhashed_key = UK, 
           type = Type},
    dict:store(UK, R, DB).

add(Type, K, UK, Val, DB) ->
    error = dict:find(UK, DB),
    R = #consensus_state{empty = false, val = Val, key = K, unhashed_key = UK, type = Type},
    dict:store(UK, R, DB).

update(UK, Val, DB) ->
    %io:fwrite({K, dict:fetch_keys(DB)}),
    {ok, R} = dict:find(UK, DB),
    R2 = R#consensus_state{empty = false, val = Val},
    dict:store(UK, R2, DB).

read2(UK, DB) ->
    dict:find(UK, DB).

read(UK, DB) ->
    case dict:find(UK, DB) of
        error -> error;
        {ok, R} ->
            if
                not(is_record(R, consensus_state)) -> io:fwrite(R);
                true -> ok
            end,
            case R#consensus_state.empty of
                true -> {empty, R#consensus_state.type};
                false -> {ok, R#consensus_state.type, R#consensus_state.val}
            end
    end.
            
test() ->    
    DB = new_db(),
    error = is_empty(1, DB),
    error = read(1, DB),
    DB2 = add_empty(a, 1, one, DB),
    {empty, a} = read(one, DB2),
    true = is_empty(one, DB2),
    DB3 = add(a, 2, two, "two", DB2),
    false = is_empty(two, DB3),
    true = is_empty(one, DB3),
    DB4 = update(one, "one", DB3),
    false = is_empty(one, DB4),
    {ok, a, "two"} = read(two, DB4),
    {ok, a, "one"} = read(one, DB4),
    success.
                 
            

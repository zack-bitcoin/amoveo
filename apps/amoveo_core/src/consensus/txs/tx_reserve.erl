-module(tx_reserve).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,

         clean/1, %scans all txs, removes if they were created more than 10 blocks ago.
         add/2, %do this every time you add a tx to the tx pool. remember the height you first saw it at, store by tx hash.
         in_block/1, %remove a list of txs because they were already included in a block.
         all/0, %returns all the valid txs 
         all/1, %returns everything
         dump/0,
         tx_hash/1
]).
-include("../../records.hrl").

-define(save_gap, 10).
-define(on, true).

-record(db, {l = [], d = dict:new()}).

init(ok) -> {ok, #db{}}.%list of tx sorted by the order we found out about them in.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({clean, Height}, DB) -> 
    {L2, Removed} = cut_tail2(Height - ?save_gap, DB#db.l, []),
    D2 = remove_all_dict(DB#db.d, Removed),
    DB2 = DB#db{l = L2, d = D2},
    {noreply, DB2};
handle_cast({in_block, TxHashes}, DB) -> 
%    ExistingHashes = lists:map(fun({_, Hash, _}) -> Hash end, X),
%    TxHashes2 = lists:sort(TxHashes),
    
%    ExistingHashes2 = lists:sort(ExistingHashes),

%    Overlap = overlap_in_sorted_hashes(TxHashes2,ExistingHashes2),
%    X2 = remove_overlap(Overlap, X),
    X = DB#db.l,
    {X2, Removed} = remove_overlap(TxHashes, X),
    D2 = remove_all_dict(DB#db.d, Removed),
    DB2 = DB#db{l = X2, d = D2},
    {noreply, DB2};
handle_cast({add, SignedTx, TxHash, Height}, DB) -> 
    X = DB#db.l,
    Dict = DB#db.d,
    case dict:find(TxHash, Dict) of
        error ->
            L2 = [{SignedTx, TxHash, Height}|X],
            Dict2 = dict:store(TxHash, ok, Dict),
            DB2 = DB#db{l = L2, d = Dict2},
            {noreply, DB2};
        _ ->
            {noreply, DB}
    end;
handle_cast(dump, _) -> 
    {noreply, #db{}};
handle_cast(_, X) -> {noreply, X}.
handle_call(all, _From, X) -> 
    {reply, X#db.l, X};
handle_call({all, 1}, _From, X) -> 
    {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.

sort_tx_hashes(L) ->
    lists:sort(L).
sort_existing_hashes(L) ->
    lists:sort(fun(X = {_, XHash, _},
                   Y = {_, YHash, _}) ->
                       XHash < YHash
               end, L).
%overlap_in_sorted_hashes([], _) ->
%    [];
%overlap_in_sorted_hashes(_, []) ->
%    [];
%overlap_in_sorted_hashes([TxHash|T1], [TxHash|T2]) ->
%    [TxHash|overlap_in_sorted_hashes(T1, T2)];
%overlap_in_sorted_hashes([TxHash|T2], X = [TxHash2|_]) 
%  when TxHash < TxHash2 ->
%    overlap_in_sorted_hashes(T2, X);
%overlap_in_sorted_hashes(T1 = [TxHash|_], [TxHash2|T2]) 
%  when TxHash > TxHash2 ->
%    overlap_in_sorted_hashes(T1, T2).

remove_overlap(L, X) ->
    D = lists:foldl(fun(H, A) ->
                           dict:store(H, ok, A)
                   end, dict:new(), L),
    X2 = lists:filter(fun({_, H, _}) ->
                              error == dict:find(H, D)
                      end, X),
    Y2 = lists:filter(fun({_, H, _}) ->
                              not(error == dict:find(H, D))
                      end, X),
    {X2, Y2}.

%cut_tail(_, []) -> [];
%cut_tail(Height, [{_, _, H}|T])
%  when H < Height -> [];
%cut_tail(H, [X|T]) ->
%    [X|cut_tail(H, T)].

cut_tail2(Height, [], A) ->
    {lists:reverse(A), []};
cut_tail2(Height, [{_, _, H}|T], A)
  when H < Height ->
    {lists:reverse(A), T};
cut_tail2(H, [X|T], A) ->
    cut_tail2(H, T, [X|A]).

remove_all_dict(D, []) -> D;
remove_all_dict(D, [{STx, TxHash, Height}|T]) ->
    D2 = dict:erase(TxHash, D),
    remove_all_dict(D2, T).
    

tx_hash(Tx) when element(1, Tx) == signed ->
    D = Tx#signed.data,
    tx_hash(D);
tx_hash(Tx) ->
    hash:doit(sign:serialize(Tx)).





clean(Height) -> 
    if
        ?on ->
            gen_server:cast(?MODULE, {clean, Height});
        true -> ok
    end.
add(Tx, Height) ->
    if
        ?on ->
    true = is_tuple(Tx),
    true = is_integer(Height),
    TxHash = tx_hash(Tx),
            gen_server:cast(?MODULE, {add, Tx, TxHash, Height});
        true -> ok
    end.
in_block(Txs) ->
    TxHashes = lists:map(fun(Tx) ->
                                 tx_hash(Tx)
                         end, Txs),
    if
        ?on ->
            gen_server:cast(?MODULE, {in_block, TxHashes});
        true -> ok
    end.
            
all(1) ->
    A = gen_server:call(?MODULE, {all, 1}),
    lists:map(fun({S, _H}) -> S end, A).
all() ->
    if
        ?on ->
            A = gen_server:call(?MODULE, all),
            lists:map(fun({S, _H}) -> S end, A);
        true -> ok
    end.
dump() ->
    if
        ?on ->
            gen_server:cast(?MODULE, dump);
        true -> ok
    end.
    

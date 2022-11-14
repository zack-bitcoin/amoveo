-module(verkle_trees_sup).
-behaviour(supervisor).
-export([start_link/0,init/1, stop/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dumps() ->
    [{accounts_dump, 45},
     {exist_dump, 40},
     {oracle_dump, 163},
     {matched_dump, 89},
     {unmatched_dump, 105},
     {sub_acc_dump, 81},
     {contract_dump, 153},
     {trade_dump, 36},
     {market_dump, 124},
     {receipts_dump, 73}].
    
stop() -> 
    %todo, kill the child processes.
    %supervisor:terminate_child(amoveo_sup, H),
    %trie_sup:stop(H),
    verkle_sup:stop(verkle),
    lists:map(fun({N, _}) ->
                      dump_sup:stop(N)
              end, dumps()).

dump_child_maker(Name, Size) ->
    {Name, {dump_sup, start_link,[Name, Size, 0, hd, ""]}, permanent, 5000, supervisor, [dump_sup]}.

init([]) ->
    %need a dump for each kind of thing being stored, and one verkle tree with 32-byte leaves.
    %first byte is the type, the next 7 bytes store a pointer. 72 quadrillion possible values it can point to. so each person on earth can have millions of accounts.
    MetaBytes = 8,
    %Children = [{verkle_supervisor, {verkle_sup, start_link, [32, 32, verkle, 0, MetaBytes, hd, "data/verkle.db"]}, permanent, 5000, supervisor, [verkle_sup]},
    DChildren = 
        lists:map(
          fun({N, V}) ->
                     dump_child_maker(N, V)
             end, dumps()),
    Children = [{verkle_supervisor, {verkle_sup, start_link, [32, 32, amoveo, 0, MetaBytes, hd, ""]}, permanent, 5000, supervisor, [verkle_sup]}] ++ DChildren,
                %{A1, {dump_sup, start_link,[A1, constants:account_size(), 0, hd, "data/account_verkle_dump.db"]}, permanent, 5000, supervisor, [dump_sup]}],
                %{A1, {dump_sup, start_link,[A1, 44, 0, hd, ""]}, permanent, 5000, supervisor, [dump_sup]}],
    {ok, {{one_for_one, 50000, 1}, Children}}.

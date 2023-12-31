-module(verkle_trees_sup).
-behaviour(supervisor).
-export([start_link/0,init/1, stop/0, mode/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dumps() ->
    [
     {accounts_dump, 45},
     {oracles_dump, 139},
     {matched_dump, 89},
     {unmatched_dump, 106},
     {sub_accs_dump, 81},
     {contracts_dump, 153},
     {trades_dump, 36},
     {markets_dump, 124},
     {receipts_dump, 69},
     {jobs_dump, 126},
     {futarchy_dump, 346},
     {futarchy_unmatched_dump, 147},
     {futarchy_matched_dump, 115}
    ].
dumps2() ->
    [
     {accounts_cleaner, 45},
     {oracles_cleaner, 139},
     {matched_cleaner, 89},
     {unmatched_cleaner, 106},
     {sub_accs_cleaner, 81},
     {contracts_cleaner, 153},
     {trades_cleaner, 36},
     {markets_cleaner, 124},
     {receipts_cleaner, 69},
     {jobs_cleaner, 126},
     {futarchy_cleaner, 346},
     {futarchy_unmatched_cleaner, 147},
     {futarchy_matched_cleaner, 115}
].
    
stop() -> 
    %todo, kill the child processes.
    %supervisor:terminate_child(amoveo_sup, H),
    %trie_sup:stop(H),
    verkle_sup:stop(verkle),
    lists:map(fun({N, _}) ->
                      dump_sup:stop(N)
              end, dumps()).

mode() ->
    case application:get_env(amoveo_core, trie_mode) of
        {ok, ram} -> ram;
        {ok, hd} -> hd;
        _ -> hd
    end.
    %trie_mode
    %ram.
    %hd.

dump_child_maker(Name, Size, Location) ->
    %Location = "",
    %Location = constants:custom_root(),
    {Name, {dump_sup, start_link,[Name, Size, 0, mode(), Location]}, permanent, 5000, supervisor, [dump_sup]}.

init([]) ->
    %need a dump for each kind of thing being stored, and one verkle tree with 32-byte leaves.
    %first byte is the type, the next 7 bytes store a pointer. 72 quadrillion possible values it can point to. so each person on earth can have millions of accounts.
    MetaBytes = 8,
    %Children = [{verkle_supervisor, {verkle_sup, start_link, [32, 32, verkle, 0, MetaBytes, hd, "data/verkle.db"]}, permanent, 5000, supervisor, [verkle_sup]},
    Location = constants:custom_root(),
    CleanFolder = "cleaner/",
    DChildren = 
        lists:map(
          fun({N, V}) ->
                     dump_child_maker(N, V, Location)
             end, dumps()),
    DChildren2 = 
        lists:map(
          fun({N, V}) ->
                     dump_child_maker(N, V, CleanFolder)
             end, dumps2()),
    %Location = "",
    Children = [
                {verkle_supervisor, {verkle_sup, start_link, [32, 32, amoveo, 0, MetaBytes, mode(), Location]}, permanent, 5000, supervisor, [verkle_sup]},
                {verkle_cleaner, {verkle_sup, start_link, [32, 32, cleaner, 0, MetaBytes, mode(), CleanFolder]}, permanent, 5000, supervisor, [verkle_sup]}
               ] ++ DChildren ++ DChildren2,
                %{A1, {dump_sup, start_link,[A1, constants:account_size(), 0, hd, "data/account_verkle_dump.db"]}, permanent, 5000, supervisor, [dump_sup]}],
                %{A1, {dump_sup, start_link,[A1, 44, 0, hd, ""]}, permanent, 5000, supervisor, [dump_sup]}],
    {ok, {{one_for_one, 50000, 1}, Children}}.

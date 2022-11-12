-module(verkle_trees_sup).
-behaviour(supervisor).
-export([start_link/0,init/1, stop/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() -> 
    %todo, kill the child processes.
    %supervisor:terminate_child(amoveo_sup, H),
    %trie_sup:stop(H),
    A1 = accounts_dump,
    verkle_sup:stop(verkle),
    dump_sup:stop(A1),
    ok.

init([]) ->
    %need a dump for each kind of thing being stored, and one verkle tree with 32-byte leaves.
    %first byte is the type, the next 7 bytes store a pointer. 72 quadrillion possible values it can point to. so each person on earth can have millions of accounts.
    MetaBytes = 8,
    A1 = accounts_dump,
    %Children = [{verkle_supervisor, {verkle_sup, start_link, [32, 32, verkle, 0, MetaBytes, hd, "data/verkle.db"]}, permanent, 5000, supervisor, [verkle_sup]},
    Children = [{verkle_supervisor, {verkle_sup, start_link, [32, 32, amoveo, 0, MetaBytes, hd, ""]}, permanent, 5000, supervisor, [verkle_sup]},
                %{A1, {dump_sup, start_link,[A1, constants:account_size(), 0, hd, "data/account_verkle_dump.db"]}, permanent, 5000, supervisor, [dump_sup]}],
                {A1, {dump_sup, start_link,[A1, 44, 0, hd, ""]}, permanent, 5000, supervisor, [dump_sup]}],
    {ok, {{one_for_one, 50000, 1}, Children}}.

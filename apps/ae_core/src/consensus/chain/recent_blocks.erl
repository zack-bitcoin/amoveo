-module(recent_blocks).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/0, add/3]).
-include("../../records.hrl").
-define(LOC, constants:recent_blocks()).
-record(r, {blocks = [], work = 0, save_limit = 0}).
%We keep a record of the blocks with the heighest accumulative difficulty so that we know what we will need to prune.
%If a fork starts from before fork_tolerance, then it would be growing from a block that is not recorded in this module. Since the data is pruned, you would be unable to maintain the database. So the node would freeze, and you would have to either restart syncing from the genesis block, or download and verify all the consensus data you don't have from the fork.
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if 
	     X == "" -> #r{};
	     true -> X
	 end,
    {ok, #r{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC, K),
    io:fwrite("recent blocks died!\n"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({add, Hash, TotalWork, Height}, _, X) ->
    {ok, RBP} =application:get_env(ae_core, recent_blocks_period),
    R=if
          (TotalWork > X#r.work) and ((Height rem RBP) == 0) ->
              {ok, FT} = application:get_env(ae_core, fork_tolerance),%we should look up the forth torlerance'th ancestor of the block, it's accumulative difficulty is the value we want. not Height-FT
              AB = block:get_by_height(max(0, Height - FT)),
              {ok, H} = headers:read(block:hash(AB)),
              AncestorsWork = H#header.accumulative_difficulty,
              Blocks = remove_before(X#r.blocks, AncestorsWork),
              #r{blocks = [{Hash, TotalWork}|Blocks], work = TotalWork, save_limit = AncestorsWork};
          TotalWork > X#r.save_limit ->
              X#r{blocks = [{Hash, TotalWork}|X#r.blocks]};
          true -> X 
      end,
    {reply, ok, R};
handle_call(read, _From, X) -> 
    Y = get_hashes(X#r.blocks),
    GH = block:hash(block:get_by_height(0)),
    {reply, [GH|Y], X};
handle_call(_, _From, X) -> {reply, X, X}.

get_hashes([]) -> [];
get_hashes([{Hash, _}|T]) -> 
    [Hash|get_hashes(T)].
remove_before([], _) -> [];
remove_before([{Hash, Height}|T], X) when Height < X ->
    remove_before(T, X);
remove_before([H|T], X) -> [H|remove_before(T, X)].


add(Hash, Work, Height) ->
    gen_server:call(?MODULE, {add, Hash, Work, Height}).
read() -> gen_server:call(?MODULE, read).

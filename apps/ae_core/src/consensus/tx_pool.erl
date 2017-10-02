-module(tx_pool).
-behaviour(gen_server).

%% This module holds the txs ready for the next block.
%% It needs to use txs:digest to keep track of the Accounts and Channels dicts.
%% This module needs to be ready to share either of those dicts.

%% API
-export([data/0,
         dump/0,
         absorb_tx/3,
         absorb/2]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([txs/1, trees/1, dict/1, facts/1, height/1, data_new/0, new_trees/1]).
-record(f, {txs = [],
            trees,
            new_trees,
            dict = dict:new(),
            facts = [],
            height = 0}).

%% API functions
txs(F) -> F#f.txs.
trees(F) -> F#f.trees.
new_trees(F) -> F#f.new_trees.
dict(F) -> F#f.dict.
facts(F) -> F#f.facts.
height(F) -> F#f.height.

data_new() ->
    gen_server:call(?MODULE, data_new).
data() ->
    {Trees, Height, Txs} = gen_server:call(?MODULE, data),
    {Trees, Height, Txs}.

dump() ->
    gen_server:call(?MODULE, dump).

absorb_tx(Trees, NewDict, Tx) ->
    gen_server:call(?MODULE, {absorb_tx, Trees, NewDict, Tx}).

absorb(Trees, Height) ->
    gen_server:call(?MODULE, {absorb, Trees, Height}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).


%% gen_server callbacks

init(ok) ->
    lager:info("~p started", [?MODULE]),
    State = initial_state(),
    io:fwrite("blockchain ready\n"),
    {ok, State}.

handle_call(dump, _From, _OldState) ->
    State = current_state(),
    {reply, 0, State};
handle_call({absorb_tx, NewTrees, NewDict, Tx}, _From, F) ->
    NewTxs = [Tx | F#f.txs],
    BlockSize = size(term_to_binary(NewTxs)),
    Governance = trees:governance(NewTrees),
    MaxBlockSize = governance:get_value(max_block_size, Governance),
    F2 = case BlockSize > MaxBlockSize of
             true ->
                 lager:warning("Cannot absorb tx - block is already full"),
                 F;
             false ->
                 F#f{txs = NewTxs, trees = NewTrees, dict = NewDict}
         end,
    {reply, 0, F2};
handle_call({absorb, NewTrees, Height}, _From, _) ->
    {reply, 0, #f{txs = [], trees = NewTrees, new_trees = NewTrees, height = Height}};
handle_call(data_new, _From, F) ->
    {reply, F, F};
handle_call(data, _From, F) ->
    {ok, Header} = headers:read(block:hash(headers:top())),
    H = F#f.height,
    {reply, {F#f.trees, H, lists:reverse(F#f.txs)}, F}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internals

initial_state() ->
    io:fwrite("initialize 1\n"),
    Header = block:initialize_chain(),
    io:fwrite("initialize state "),
    io:fwrite(packer:pack(Header)),
    io:fwrite("\n"),
    state2(Header).
current_state() ->
    Header = headers:top(),
    state2(Header).
state2(Header) ->
    Block = block:get_by_hash(block:hash(Header)),
    case Block of
	empty -> 
	    {ok, PrevHeader} = headers:read(headers:prev_hash(Header)),
	    state2(PrevHeader);
	_ ->
	    #f{trees = block:trees(Block),
	       height = block:height(Block)}
    end.

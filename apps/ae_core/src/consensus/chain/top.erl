-module(top).
-behaviour(gen_server).

%% API
-export([%add/1,
         %doit/0
	]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(LOC, constants:top()).

%% API functions

add(Block) ->
    gen_server:call(?MODULE, {add, Block}).

doit() ->
    gen_server:call(?MODULE, top).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).


%% gen_server callbacks

init(ok) ->
    lager:info("Start ~p", [?MODULE]),
    io:fwrite("top init 00\n"),
    GenesisMakerBlock = block:genesis_maker(),
    GHeader = block:block_to_header_new(GenesisMakerBlock),
    GBH = block:hash(block:block(GenesisMakerBlock)),
    ok = block_absorber:save_helper(GenesisMakerBlock),
    spawn(fun() ->
		  gen_server:call(headers, {add, testnet_hasher:doit(GHeader),
					    GHeader, 0})
	  end),
    Top = db:read(?LOC),
    TopHash =
        case Top == "" of
            true ->
                GBH;
	    false ->
                Top
        end,
    {ok, TopHash}.

handle_call({add, Block}, _From, OldBlockHash) ->
    %% Check which block is higher and store it's hash as the top.
    %% For tiebreakers, prefer the older block.
    {_,_,OldTxs} = tx_pool:data(),
    OldBlock = block:read(OldBlockHash),
    NH = block:height(Block),
    OH = block:height(OldBlock),
    NewBlockHash =
        case NH > OH of
            true ->
                Trees = block:trees(Block),
                NH = block:height(Block),
                tx_pool:absorb(Trees, [], NH),
                NBH = hash_and_save(Block),
		tx_pool_feeder:absorb(OldTxs),
		NBH;
            false ->
                OldBlockHash
        end,
    {reply, 0, NewBlockHash};
handle_call(_, _From, BlockHash) ->
    {reply, BlockHash, BlockHash}.

handle_cast({set_top, TH}, _State) ->
    {noreply, TH};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internals
set_top(TH) ->
    gen_server:cast(?MODULE, {set_top, TH}).
save_hash(BlockHash) ->
    ok = db:save(?LOC, BlockHash).

hash_and_save(Block) ->
    BlockHash = block:hash(Block),
    ok = save_hash(BlockHash),
    BlockHash.

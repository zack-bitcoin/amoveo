-module(top).
-behaviour(gen_server).

%% API
-export([add/1,
         doit/0]).

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
    GenesisMakerBlock = block:genesis_maker(),
    GenesisMakerBlockHash = block:hash(GenesisMakerBlock),
    Top = db:read(?LOC),
    TopHash =
        case Top == "" of
            true ->
                ok = block_absorber:save_helper(GenesisMakerBlock),
                ok = save_hash(GenesisMakerBlockHash),
                GenesisMakerBlockHash;
            false ->
                Top
        end,
    spawn(fun() ->
                  block_hashes:add(GenesisMakerBlockHash)
          end),
    {ok, TopHash}.

handle_call({add, Block}, _From, OldBlockHash) ->
    %% Check which block is higher and store it's hash as the top.
    %% For tiebreakers, prefer the older block.

    OldBlock = block:read(OldBlockHash),
    NH = block:height(Block),
    OH = block:height(OldBlock),
    NewBlockHash =
        case NH > OH of
            true ->
                Trees = block:trees(Block),
                NH = block:height(Block),
                tx_pool:absorb(Trees, [], NH),
                _NewBlockHash = hash_and_save(Block);
            false ->
                OldBlockHash
        end,
    {reply, 0, NewBlockHash};
handle_call(_, _From, BlockHash) ->
    {reply, BlockHash, BlockHash}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internals

save_hash(BlockHash) ->
    ok = db:save(?LOC, BlockHash).

hash_and_save(Block) ->
    BlockHash = block:hash(Block),
    ok = save_hash(BlockHash),
    BlockHash.

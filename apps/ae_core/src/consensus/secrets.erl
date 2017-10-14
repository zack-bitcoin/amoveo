%% When we sign a block, we record the hash of a secret. Later on, we need to reveal this secret.
%% This module holds a bunch of secrets make by this node, stored in a dict by hash.

%% Needs garbage collection.

-module(secrets).
-behaviour(gen_server).

%% API
-export([add/2,
         read/1,
         delete/1,
         new_lightning/0,
         check/0]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(LOC, constants:secrets()).
-define(none, <<"none">>).

%% API functions

add(Code, SS) ->
    gen_server:cast(?MODULE, {add, Code, SS}).

read(Code) ->
    gen_server:call(?MODULE, {read, Code}).

delete(SH) ->
    gen_server:cast(?MODULE, {del, SH}).

new_lightning() ->
    %delay for canceling is 100
    S = crypto:strong_rand_bytes(constants:hash_size()),
    SH = testnet_hasher:doit(S),
    ESH = "drop stack_size int 0 == if
int 100 int 1 int 0 crash else then  drop drop
hash binary " ++ integer_to_list(constants:hash_size())++ " " ++
	binary_to_list(base64:encode(SH)) ++
	" print == swap drop swap drop if
int 0 int 2 int 10000
else
int 100 int 1 int 0 then crash",
    ESS = "binary " ++ integer_to_list(constants:hash_size()) ++ " " ++ base64:encode(S),
    Code = compiler_chalang:doit(list_to_binary(ESH)),
    SS = spk:new_ss(compiler_chalang:doit(list_to_binary(ESS)), []),
    {Trees, Height, _} = tx_pool:data(),%for sanity check
    Amount = 200,
    Bet = spk:new_bet(Code, Code, Amount),
    SPK = spk:new(1, 2, 3, [Bet], 9000, 9000, 1, 1, 1),
    {Amount, _, _} = spk:run(fast, [SS], SPK, Height, 0, Trees),%for sanity check
    add(Code, SS),
    {Code, SS}.

check() ->
    gen_server:call(?MODULE, check).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).


%% gen_server callbacks

init(ok) ->
    lager:info("Starting ~p", [?MODULE]),
    process_flag(trap_exit, true),
        K = case db:read(?LOC) of
		"" ->
		     dict:new();
		X ->
            binary_to_term(X)
    end,
    {ok, K}.

handle_call(check, _From, State) ->
    {reply, State, State};
handle_call({read, Code}, _From, X) ->
    Packed = packer:pack({secret_read, Code}),
    lager:info("Reading secret: ~p", [Packed]),
    Z = case dict:find(Code, X) of
	    error -> ?none;
	    {ok, Y} -> Y
	end,
    {reply, Z, X}.

handle_cast({add, Code, ?none}, X) ->
    {noreply, dict:erase(Code, X)};
handle_cast({add, Code, SS}, X) ->
    Packed = packer:pack({secret_add, Code, SS}),
    lager:info("Adding secret: ~p", [Packed]),
    {noreply, dict:store(Code, SS, X)};
handle_cast({delete, Code}, X) ->
    {noreply, dict:erase(Code, X)}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    db:save(?LOC, term_to_binary(State)),
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

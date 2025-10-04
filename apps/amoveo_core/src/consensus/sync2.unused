%The goal 
% to replace sync:start, sync:get_headers, checkpoint:sync, checkpoint:reverse_sync. Everything we do manually while syncing.
%Also, we want to fix some race conditions:
% * don't pull blocks from multiple peers at once.
% * don't sync and build checkpoints at the same time.

% It also has a cron job to make decisions about how to stay in sync.

% a fundamental problem with syncing is that  we can receive bad data from a peer. The thread can crash, and we need to handle that kind of crash.
% we don't want sync2 to crash, so the interaction with a peer of getting blocks from them and verifying those blocks needs to happen on it's own thread.
% but, if sync2 is starting a new thread, and waiting for it to respond. How can sync2 realize when that thread has crashed, and switch to syncing with a different peer instead?

% the thread needs to regularly send the "ok" signal back to sync2. So that if there is no signal for a long enough time, then sync2 can switch to pulling blocks from a different peer.


-module(sync2).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         start/0, stop/0, status/0, 
         ok/2,
         set_forward_id/1, set_reverse_id/1, set_headers_id/1]).

-record(d, {status = go, 
            synced_checkpoints = false,
            headers_timestamp = 0, %when we last found a header
            headers_check_timestamp = now(),%when the headers thread last checked in with us.
            headers_id,
            forward_timestamp = 0, %when the thread syncing with this peer last checked in with us.
            forward_id,
            checkpoint_id,%thread for syncing the checkpoint.
            checkpoint_timestamp = 0,
            reverse_id, %peer you are syncing in reverse with
            reverse_timestamp = 0}). %when the peer last checked in.
%if state is a peer, then we are syncing with that peer.

-define(verbose, true).
-define(cronPeriod, 1000).

init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(go, X) -> 
    {noreply, X#d{status = go}};
handle_cast(synced_checkpoints, X) -> 
    {noreply, X#d{synced_checkpoints = true}};
handle_cast(_, X) -> {noreply, X}.
handle_call(status, _From, X) -> 
    {reply, X#d.status, X};
handle_call(data, _From, X) -> {reply, X, X};
handle_call({ok, Type, ID}, _From, X) -> 
    {TID, Timestamp} = type2atoms(Type),
    case X#d.TID of
        ID -> {reply, ok, X#d{Timestamp = now()}};
        FID -> {reply, FID, X}
    end;
handle_call(_, _From, X) -> {reply, X, X}.

print(X) ->
    if
        ?verbose ->
            io:fwrite(X);
        true ->
            ok
    end.

type2atoms(forward) ->
    {forward_timestamp, forward_id};
type2atoms(reverse) ->
    {reverse_timestamp, reverse_id};
type2atoms(checkpoint) ->
    {checkpoint_timestamp, checkpoint_id};
type2atoms(headers) ->
    {headers_timestamp, headers_id};

ok(Type, ID) ->
    go = status(),
    type2atoms(Type),
    ID = gen_server:call(?MODULE, {ok, Type, ID}).

status() ->
    gen_server:call(?MODULE, status).

start() ->
    case status() of
        go ->
            io:fwrite("already syncing."),
            ok;
        stop ->
            B = block:height(),
            case block:height() of
                0 -> io:fwrite("start syncing.");
                _ -> 
                    io:fwrite("resume syncing."),
                    gen_server:cast(?MODULE, go),
                    cron()
            end
    end.
stop() ->
    case status() of
        go ->
            io:fwrite("stopping."),
            gen_server:cast(?MODULE, stop);
        stop ->
            io:fwrite("already stopped.")
    end.

            
cron() ->
    timer:sleep(?cronPeriod),
    S = status(),
    case status() of
        go ->
            spawn(fun() -> think() end),
            cron();
        stop ->
            ok
    end.

now() ->
    erlang:monotonic_time(millisecond).

think() ->
    %There are several points in the syncing process we could be in. we need to identify where we are, to know what needs to get done next.
    case status() of
        stop -> ok;
        go ->
            D = gen_server:call(?MODULE, data),
            HH = api:height(),
            BH = block:height(),
            BB = block:bottom(),
            N = now(),
            HD = N - D#d.headers_timestamp,
            HCD = N - D#d.headers_check_timestamp,
            CD = N - D#d.forward_timestamp,
            RD = N - D#d.reverse_timestamp,
            RCD = N - D#d.checkpoint_timestamp,

            %todo look in checkpoint:recent to figure out if it is time to build another checkpoint yet. while we are building the checkpoint, set status to "stop", to prevent any other thread from messing with the database.
            %don't make the checkpoint while syncing. Finish syncing first, and make the checkpoint after.

            if
                (HCD > 3000) ->
                    %check for new headers every 3 seconds.
                    print("sync2 thinking. check for new headers\n"),
                    %todo start headers thread
                    ok;
                (HH < 377000) and (HD < 3000) ->
                    print("sync2 thinking. wait for more headers.\n"),
                    ok;
                (HH < 377000) or ((D#synced_checkpoints == false) and (HD < 10000)) ->
                    %try pulling headers from a different peer
                    %todo start headers thread
                    ok;
                (BH == 0) and (RCD > 5000) ->
                    gen_server:cast(?MODULE, synced_checkpoints),
                    %todo start pulling the checkpoint
                    ok;
                (BH == 0) ->
                    print("sync2 thinking. wait for checkpoint to finish.\n"),
                    ok;
                (BH < HH) and (CD > 3000)->
                    %restart syncing blocks forwards
                    ok;
                (BH < HH) ->
                    print("sync2 thinking. wait for more blocks to sync\n"),
                    ok;
                (BB > 0) and (RD > 3000) ->
                    %restart syncing blocks in reverse
                    ok;
                (BB > 0) ->
                    print("sync2 thinking. wait for more blocks to sync in reverse\n"),
                    ok
    end.

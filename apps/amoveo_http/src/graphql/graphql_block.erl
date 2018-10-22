-module(graphql_block).
-include("../../amoveo_core/src/records.hrl").

-export([execute/4]).

execute(_Ctx, Block, Field, Args) ->
    case Field of
        <<"height">> ->
          {ok, Block#block.height};
        <<"prev_hash">> ->
          {ok, Block#block.prev_hash};
        <<"trees_hash">> ->
          {ok, Block#block.trees_hash};
        <<"time">> ->
          {ok, Block#block.time};
        <<"difficulty">> ->
          {ok, Block#block.difficulty};
        <<"period">> ->
          {ok, Block#block.period};
        <<"version">> ->
          {ok, Block#block.version};
        <<"nonce">> ->
          {ok, Block#block.nonce};
        <<"market_cap">> ->
          {ok, Block#block.market_cap};
        <<"channels_veo">> ->
          {ok, Block#block.channels_veo};
        <<"live_channels">> ->
          {ok, Block#block.live_channels};
        <<"many_accounts">> ->
          {ok, Block#block.many_accounts};
        <<"many_oracles">> ->
          {ok, Block#block.many_accounts};
        <<"live_oracles">> ->
          {ok, Block#block.many_accounts}
    end.

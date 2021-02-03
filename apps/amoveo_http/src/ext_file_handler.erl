-module(ext_file_handler).

-export([init/3, init/2, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
init(Req, Opts) ->
	handle(Req, Opts).
handle(Req, State) ->
    F = cowboy_req:path(Req),
    PrivDir0 = 
	case application:get_env(amoveo_core, kind) of
	    {ok, "production"} ->
		"../../../../_build/prod/lib/light_node/src/js";
	    {ok, "local"} ->
		"../../../../_build/local/lib/light_node/src/js";
	    {ok, "integration"} ->
		"../../../../_build/dev1/lib/light_node/src/js"
	end,
    PrivDir = list_to_binary(PrivDir0),
    B = case F of
            <<"/peer_scan.html">> -> true;
            <<"/peer_scan.js">> -> true;
            <<"/channels_interface.js">> -> true;
            <<"/channels_lookup.js">> -> true;
            <<"/governance.js">> -> true;
            <<"/glossary.js">> -> true;
            <<"/home.html">> -> true;
            <<"/txs.html">> -> true;
            <<"/oracle_close.html">> -> true;
            <<"/oracle_close.js">> -> true;
            <<"/oracle_bet.html">> -> true;
            <<"/oracle_bet.js">> -> true;
            <<"/encryption.html">> -> true;
            <<"/lookup.html">> -> true;
            <<"/channel_with_server.html">> -> true;
            <<"/new_oracle.html">> -> true;
            <<"/new_oracle.js">> -> true;
            <<"/oracle_bet.html">> -> true;
            <<"/oracle_bet.js">> -> true;
            <<"/oracle_winnings.html">> -> true;
            <<"/oracle_winnings.js">> -> true;
            <<"/oracle_close.html">> -> true;
            <<"/oracle_close.js">> -> true;
            <<"/messenger.js">> -> true;
            <<"/otc_finisher.html">> -> true;
            <<"/otc_finisher_old.html">> -> true;
            <<"/otc_finisher_old.js">> -> true;
            <<"/otc_finisher.js">> -> true;
            <<"/otc_finisher2.js">> -> true;
            <<"/otc_listener.js">> -> true;
            <<"/otc_listener2.js">> -> true;
            <<"/otc_listener.html">> -> true;
            <<"/otc_derivatives.html">> -> true;
            <<"/otc_derivatives.js">> -> true;
            <<"/otc_derivatives2.js">> -> true;
            <<"/sign_tx.js">> -> true;
            <<"/secrets.js">> -> true;
            <<"/js_loader.js">> -> true;
            <<"/explorer_title.js">> -> true;
            <<"/lightning.js">> -> true;
            <<"/encryption.js">> -> true;
            <<"/encryption_interface.js">> -> true;
            <<"/encryption_library.js">> -> true;
            <<"/encryption.html">> -> true;
            <<"/finance_game.html">> -> true;
            <<"/finance_game.js">> -> true;
            <<"/human_language.js">> -> true;
            <<"/title.js">> -> true;
            <<"/bets.js">> -> true;
            <<"/miner.js">> -> true;
            <<"/chalang.js">> -> true;
            <<"/spk.js">> -> true;
            <<"/format.js">> -> true;
            <<"/files.js">> -> true;
            <<"/rpc.js">> -> true;
            <<"/oracles.js">> -> true;
            <<"/oracle_list.js">> -> true;
            <<"/active_oracles.js">> -> true;
            <<"/channels.js">> -> true;
            <<"/headers.js">> -> true;
            <<"/server.js">> -> true;
            <<"/codecBytes.js">> -> true;
            <<"/height.js">> -> true;
            <<"/sha256.js">> -> true;
            <<"/combine_cancel_assets.js">> -> true;
            <<"/hexbase64.js">> -> true;
            <<"/signing.js">> -> true;
            <<"/create_account.js">> -> true;
            <<"/delete_account.js">> -> true;
            <<"/keys.js">> -> true;
            <<"/keys2.js">> -> true;
            <<"/sjcl.js">> -> true;
            <<"/crypto.js">> -> true;
            <<"/lookup_account.js">> -> true;
            <<"/create_account_tx.js">> -> true;
            <<"/delete_account_tx.js">> -> true;
            <<"/spend_tx.js">> -> true;
            <<"/elliptic.min.js">> -> true;
            <<"/lookup_block.js">> -> true;
            <<"/explorer.html">> -> true;
            <<"/lookup_oracle.js">> -> true;
            <<"/total_coins.js">> -> true;
            <<"/favicon.ico">> -> true;
            <<"/market.js">> -> true;
            <<"/unused.js">> -> true;
            <<"/merkle_proofs.js">> -> true;
            <<"/wallet.html">> -> true;
            <<"/BigInteger.js">> -> true;
            <<"/big_int_test.js">> -> true;
            <<"/logo_512x512.png">> -> true;
            <<"/add_modifications.js">> -> true;
            <<"/glossary2.js">> -> true;
            <<"/trading.html">> -> true;
            <<"/loop.js">> -> true;
            <<"/utils.js">> -> true;
            <<"/add.js">> -> true;
            <<"/contracts.html">> -> true;
            <<"/contracts_list.html">> -> true;
            <<"/contracts_list.js">> -> true;
            <<"/binary_id.js">> -> true;
            <<"/resolve_binary_contract.js">> -> true;
            <<"/binary_contract_winnings.js">> -> true;
            <<"/scalar_contract_winnings.js">> -> true;
            <<"/scalar_id.js">> -> true;
            <<"/scalar_oracle_creation.js">> -> true;
            <<"/simplified_scalar_oracle_creation.js">> -> true;
            <<"/resolve_scalar_contract.js">> -> true;
            <<"/simplified_resolve_scalar_contract.js">> -> true;
            <<"/resolve_scalar_winnings.js">> -> true;
            <<"/subcurrency_balance.js">> -> true;
            <<"/subcurrency_spender.js">> -> true;
            <<"/subcurrency_combiner.js">> -> true;
            <<"/subcurrency_set_buy.js">> -> true;
            <<"/explore_swap_offers.js">> -> true;
            <<"/swap_viewer.js">> -> true;
            <<"/swap_offer.js">> -> true;
            <<"/publish_swap_offer.js">> -> true;
            <<"/check_binary_contract.js">> -> true;
            <<"/teach_binary_contract.js">> -> true;
            <<"/teach_scalar_contract.js">> -> true;
            <<"/market_viewer.js">> -> true;
            <<"/market_swap.js">> -> true;
            <<"/market_liquidity_balance.js">> -> true;
            <<"/market_liquidity.js">> -> true;
            <<"/new_market.js">> -> true;
            <<"/new_contract.js">> -> true;
            <<"/new_scalar_contract.js">> -> true;
            <<"/multi_tx.js">> -> true;
            <<"/uniswap.js">> -> true;
            <<"/uniswap.html">> -> true;
            <<"/simple_wallet.html">> -> true;
            <<"/binary_derivative.js">> -> true;
            <<"/scalar_derivative.js">> -> true;
            <<"/swaps.js">> -> true;
            <<"/sub_accounts.js">> -> true;
            <<"/pool_tab_builder.js">> -> true;
            <<"/swap_tab_builder.js">> -> true;
            <<"/spend_tab_builder.js">> -> true;
            <<"/create_tab_builder.js">> -> true;
            <<"/create_binary_tab_builder.js">> -> true;
            <<"/create_scalar_tab_builder.js">> -> true;
            <<"/create_futarchy_tab_builder.js">> -> true;
            <<"/crosschain_tab_builder.js">> -> true;
            <<"/tabs.js">> -> true;
            <<"/contract_explorer.js">> -> true;
            <<"/contract_explorer.html">> -> true;
            <<"/market_explorer.js">> -> true;
            <<"/market_explorer.html">> -> true;
            <<"/account_explorer.js">> -> true;
            <<"/account_explorer.html">> -> true;
            <<"/tx_explorer.js">> -> true;
            <<"/tx_explorer.html">> -> true;
            X -> 
                io:fwrite("ext file handler unknown page: "),
                io:fwrite(X),
                io:fwrite("\n"),
                false
        end,
    File = if
               B ->
    %File = << PrivDir/binary, <<"/external_web">>/binary, F/binary>>,
                   << PrivDir/binary, F/binary>>;
               true -> <<PrivDir/binary, "/home.html">>
           end,
    {ok, _Data, _} = cowboy_req:read_body(Req),
    Text = read_file(File),
    
    Headers = #{<<"content-type">> => <<"text/html">>,
                <<"Access-Control-Allow-Origin">> => <<"*">>},
    Req2 = cowboy_req:reply(200, Headers, Text, Req),
    {ok, Req2, State}.
read_file(F) ->
    {ok, File } = file:open(F, [read, binary, raw]),
    {ok, O} =file:pread(File, 0, filelib:file_size(F)),
    file:close(File),
    O.
init(_Type, Req, _Opts) -> {ok, Req, []}.
terminate(_Reason, _Req, _State) -> ok.

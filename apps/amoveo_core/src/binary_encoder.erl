
-module(binary_encoder).
-export([encode/1, decode/1, test/1]).

%unused (possibly could be more atoms, or more pages of 2-byte atoms)
%==========
%negative zero, 
%negative big zero, 
%big_int with magnitude zero, 
%big_neg with magnitude zero

-define(list, 0).%000
-define(tuple, 32).%001
-define(bin, 64).%010
-define(atom, 96).%011
-define(int, 128).%100
-define(neg, 160).%101
-define(big_int, 192).%110
-define(big_neg, 224).%111

-define(lim1, 256).
-define(lim2, 65536).%256*256
-define(lim3, 16777216).%256^3
-define(lim4, 4294967296).%256^4

integer_bytes(0) -> 0;
integer_bytes(A)
  when is_integer(A) and (A>0) ->
    1 + integer_bytes(A div 256).

needed_bytes(A) ->
    if
        (A < ?lim1) -> {28, 1};
        (A < ?lim2) -> {29, 2};
        (A < ?lim3) -> {30, 3};
        true -> {31, 4}
    end.

encode_list(List) ->
    L = lists:map(fun(X) -> encode(X) end,
                  List),
    T = lists:foldl(
               fun(A, B) ->
                       <<B/binary, A/binary>> end,
      <<>>, L),
    Size = size(T),
    {R, ManyBytes} = needed_bytes(Size),
    B1 = if 
             (Size < 28) -> 
                 <<(?list + Size)>>;
             true -> 
                 M8 = ManyBytes * 8,
                 <<(?list + R), Size:(M8)>>
         end,
    <<B1/binary, T/binary>>.
            
encode(X) when is_integer(X) ->
    A = abs(X),
    NegFlag = if
             (X > -1) -> 0;
             true -> 32
         end,
    Magnitude = integer_bytes(A),
    if
        (A < 28) -> %one byte int
            <<(?int + NegFlag + A)>>;
        Magnitude < 5 -> %small int
            {R2, Bytes} = needed_bytes(A),
            B8 = Bytes *8,
            <<(?int + NegFlag + R2), A:B8>>;
        (Magnitude < 28) ->  %big int
            <<(?big_int+NegFlag+Magnitude),
              A:(Magnitude*8)>>;
        (Magnitude < ?lim4) -> %big int, need more bytes for magnitude.
            Num = <<A:(Magnitude*8)>>,
            {R3, Bytes2} = needed_bytes(Magnitude),
            <<(?big_int+NegFlag+R3),
              Magnitude:(Bytes2*8),
              A:(Magnitude*8)>>;
        true ->
            {error, integer_too_big}
    end;
encode(<<0:256>>) -> 
    <<(?bin + 27)>>; %empty hash in one byte
encode(<<X:256>>) -> 
    <<(?bin + 26), X:256>>; %hashes 
encode(<<X:512>>) -> 
%signatures and uncompressed pubkeys.
    <<(?bin + 25), X:512>>;
encode(<<X:264>>) -> 
%compressed pubkey are 33 bytes
    <<(?bin + 24), X:264>>;
encode(X) when (is_binary(X) and (size(X) < 24)) ->
    <<(?bin + size(X)), X/binary>>;
encode(X) when (is_binary(X)
                and (size(X) < ?lim4))->
    S = size(X),
    {F, Bytes} = needed_bytes(S),
    <<(?bin + F), S:(Bytes*8), X/binary>>;
encode(X) when is_binary(X) ->
    {error, binary_too_big};
encode(X) when is_atom(X) ->
    I = atom2int(X),
    {S, Bytes} = needed_bytes(I),
    if
        (I == unknown) ->
            {error, unknown_atom};
        (I < 28) ->%one byte atoms
            <<(?atom + I)>>;
        (I < ?lim4) ->
            <<(?atom + S), I:(Bytes*8)>>;
        true -> {error, atom_out_of_range}
    end;
encode(X) when is_tuple(X) ->
    L = tuple_to_list(X),
    <<F, R/binary>> = encode_list(L),
    <<(F+?tuple), R/binary>>;
encode(X) when is_list(X) ->
    encode_list(X).

decode_element(<<X, R/binary>>) ->
    Mag = X rem 32,
    Type = X - Mag,
    if
        (Mag > 27) ->
            %We store 28,29,30, or 31 to indicate that the next 1,2,3, or 4 bytes store the real magnitude.
            Bytes = Mag - 27,
            B8 = Bytes*8,
            <<S:B8, R2/binary>> = R,
            decode_big(Type, S, R2);
        (((Type == ?bin) and (Mag > 23)) 
         or (Type == ?int) 
         or (Type == ?neg)
         or (Type == ?atom)) ->
            %atoms/ints that fit into one byte.
            %special binaries.
            decode2(Type, Mag, R);
        true ->
            %for when the list/binary is less than 28/24 bytes.
            B82 = Mag * 8,
            <<S2:B82, R22/binary>> = R,
            {decode3(Type, B82, <<S2:B82>>), R22}
    end.
decode_big(?int, S, R) ->
    {S, R};
decode_big(?neg, S, R) ->
    {-S, R};
decode_big(?atom, S, R) ->
    {int2atom(S), R};
decode_big(?big_int, S, R) ->
    S8 = S*8,
    <<L:S8, R2/binary>> = R,
    {L, R2};
decode_big(?big_neg, S, R) ->
    S8 = S*8,
    <<L:S8, R2/binary>> = R,
    {-L, R2};
decode_big(?bin, S, R) ->
    S8 = S*8,
    <<L:S8, R2/binary>> = R,
    {<<L:S8>>, R2};
decode_big(?list, S, R) ->
    S8 = S*8,
    <<L:S8, R2/binary>> = R,
    {decode_list(<<L:S8>>),
     R2};
decode_big(?tuple, S, R) ->
    S8 = S*8,
    <<L:S8, R2/binary>> = R,
    {decode_tuple(<<L:S8>>),
     R2}.

decode2(?bin, 24, <<X:264, R/binary>>) ->
    {<<X:264>>, R};%pubkey size
decode2(?bin, 25, <<X:512, R/binary>>) ->
    {<<X:512>>, R};%extended pubkey or signature
decode2(?bin, 26, <<X:256, R/binary>>) ->
    {<<X:256>>, R};%32 byte hash
decode2(?bin, 27, R) ->
    {<<0:256>>, R};%32 bytes of zero
decode2(?bin, N, R) ->%N is < 24
    N8 = N * 8,
    <<S:N8, R2/binary>> = R,
    {<<S:N8>>, R2};
decode2(?int, N, R) ->
    {N, R};
decode2(?neg, N, R) ->
    {-N, R};
decode2(?atom, N, R) ->
    {int2atom(N), R}.

decode3(?bin, _, Bin) -> Bin;
decode3(?list, _, Bin) -> decode_list(Bin);
decode3(?tuple, _, Bin) -> decode_tuple(Bin);
decode3(?atom, Size, Bin) -> 
    <<I:Size>> = Bin,
    int2atom(I);
decode3(?big_int, Size, Bin) ->
    <<I:Size>> = Bin,
    I;
decode3(?big_neg, Size, Bin) ->
    <<I:Size>> = Bin,
    -I.

decode_list(B) ->
    case decode_element(B) of
        {F, <<>>} -> [F];
        {G, R} ->
            [G|decode_list(R)]
    end.
decode_tuple(B) ->
    L = decode_list(B),
    list_to_tuple(L).
            

decode(B) ->
    {F, _} =  decode_element(B),
    F.
            
test(0) ->
    lists:map(fun(X) -> 
                      X = decode(encode(X)) 
              end,
              [55, -45, <<"binary">>, [1,2], 
               true, false, {1,2,3}, 
              0, ?lim1, ?lim2, ?lim3, ?lim4, 
               10000000000000000000000000, 1, [28], 
               [{20239392039029302439040923749032742}], 
               {{{[true, -7483493748374893274973298483479279437982]}}}
              ]);
test(1) ->
    %L = [[[[],[],[],[]]]],
    %L = [[[[[[[[[1]]]]]]]]],
    L = {{{{{{{{{1}}}}}}}}},
    R = range(1, 10000),
    lists:map(fun(_) -> encode(L) end, R),
    T1 = erlang:timestamp(),
    lists:map(fun(_) -> encode(tx) end, R),
    T2 = erlang:timestamp(),
    lists:map(fun(_) -> encode(true) end, R),
    T3 = erlang:timestamp(),
    lists:map(fun(_) -> encode(L) end, R),
    T4 = erlang:timestamp(),
    lists:map(fun(_) -> decode(encode(tx)) end, R),
    T5 = erlang:timestamp(),
    lists:map(fun(_) -> decode(encode(true)) end, R),
    T6 = erlang:timestamp(),
    lists:map(fun(_) -> decode(encode(L)) end, R),
    T7 = erlang:timestamp(),
    lists:map(fun(_) -> packer:pack(L) end, R),
    
    T8 = erlang:timestamp(),
    lists:map(fun(_) -> packer:unpack(packer:pack(L)) end, R),
    T9 = erlang:timestamp(),
    %lists:map(fun(_) -> jiffy:encode(L) end, R),
    
    T10 = erlang:timestamp(),
    %lists:map(fun(_) -> jiffy:decode(jiffy:encode(L)) end, R),
    T11 = erlang:timestamp(),
    {{encode_tx, timer:now_diff(T2, T1)},
     {encode_first, timer:now_diff(T3, T2)},
     {encode_nums, timer:now_diff(T4, T3)},
     {encode_decode_tx, timer:now_diff(T5, T4)},
     {encode_decode_first, timer:now_diff(T6, T5)},
     {encode_decode_nums, timer:now_diff(T7, T6)},
     {pack_nums, timer:now_diff(T8, T7)},
     {unpack_nums, timer:now_diff(T9, T8)},
     {jiffy_pack_nums, timer:now_diff(T10, T9)},
     {jiffy_unpack_nums, timer:now_diff(T11, T10)}
     }.

range(N, N) -> [N];
range(N, M) when N < M -> 
    [N|range(N+1, M)].

atom2int(true) -> ?lim2;
atom2int(false) -> ?lim2 + 1;
atom2int(contract) -> ?lim2 + 2;
atom2int(contracts) -> ?lim2 + 3;
atom2int(accounts) -> ?lim2 + 4;
atom2int(sub_accounts) -> ?lim2 + 5;
atom2int(sub_channels) -> ?lim2 + 6;
atom2int(trades) -> ?lim2 + 7;
atom2int(contract_new_tx) -> ?lim2 + 8;
atom2int(contract_use_tx) -> ?lim2 + 9;
atom2int(sub_spend_tx) -> ?lim2 + 10;
atom2int(contract_evidence_tx) -> ?lim2 + 11;
atom2int(contract_resolve_tx) -> ?lim2 + 12;
atom2int(contract_timeout_tx) -> ?lim2 + 13;
atom2int(contract_timeout_tx2) -> ?lim2 + 14;
atom2int(contract_winnings_tx) -> ?lim2 + 15;
atom2int(contract_simplify_tx) -> ?lim2 + 16;
atom2int(pair_buy_tx) -> ?lim2 + 17;
atom2int(pair_buy_offer) -> ?lim2 + 18;
atom2int(team_buy_tx) -> ?lim2 + 19;
atom2int(x) -> ?lim2 + 20;
atom2int(final_spend) -> ?lim2 + 21;
atom2int(waiver) -> ?lim2 + 22;
atom2int(owner) -> ?lim2 + 23;
atom2int(owner_layer) -> ?lim2 + 24;
atom2int(tree) -> ?lim2 + 25;
atom2int(sid) -> ?lim2 + 26;
atom2int(sid_before) -> ?lim2 + 27;
atom2int(before) -> ?lim2 + 28;
atom2int(priority) -> ?lim2 + 29;
atom2int(priority_before) -> ?lim2 + 30;
atom2int(add) -> ?lim2 + 31;
atom2int(take) -> ?lim2 + 32;
atom2int(tx_scan) -> ?lim2 + 33;
atom2int(version) -> ?lim2 + 34;
atom2int(get_offer_contract) -> ?lim2 + 35;
atom2int(get_offers) -> ?lim2 + 36;
atom2int(oracle_list) -> ?lim2 + 37;
atom2int(nc_offer) -> ?lim2 + 38;
atom2int(channel_sig) -> ?lim2 + 39;
atom2int(send) -> ?lim2 + 40;
atom2int(read) -> ?lim2 + 41;
atom2int(block_hash) -> ?lim2 + 42;
atom2int(governance) -> ?lim2 + 43;
atom2int(existence) -> ?lim2 + 44;
atom2int(new_scalar_oracle) -> ?lim2 + 45;
atom2int(scalar) -> ?lim2 + 46;
atom2int(binary) -> ?lim2 + 47;
atom2int(price) -> ?lim2 + 48;
atom2int(lookup) -> ?lim2 + 49;
atom2int(post) -> ?lim2 + 50;
atom2int(ewah) -> ?lim2 + 51;
atom2int(sync_normal) -> ?lim2 + 52;
atom2int(sync_quick) -> ?lim2 + 53;
atom2int(multi_tx) -> ?lim2 + 54;
atom2int(time_value) -> ?lim2 + 55;
atom2int(blocks) -> ?lim2 + 56;
atom2int(work) -> ?lim2 + 57;
atom2int(problem) -> ?lim2 + 58;
atom2int(mining_data) -> ?lim2 + 59;
atom2int(create_account_tx) -> ?lim2 + 60;
atom2int(spend_tx) -> ?lim2 + 61;
atom2int(combine_cancel_assets) -> ?lim2 + 62;
atom2int(cancel_trade) -> ?lim2 + 63;
atom2int(list_oracles) -> ?lim2 + 64;
atom2int(keys_status) -> ?lim2 + 65;
atom2int(keys_unlock) -> ?lim2 + 66;
atom2int(keys_new) -> ?lim2 + 67;
atom2int(halt) -> ?lim2 + 68;
atom2int(ss) -> ?lim2 + 69;
atom2int(exist) -> ?lim2 + 70;
atom2int(key) -> ?lim2 + 71;
atom2int(roots) -> ?lim2 + 72;
atom2int(roots2) -> ?lim2 + 73;
atom2int(roots3) -> ?lim2 + 74;
atom2int(roots4) -> ?lim2 + 75;
atom2int(roots5) -> ?lim2 + 76;
atom2int(proof) -> ?lim2 + 77;
atom2int(coinbase) -> ?lim2 + 78;
atom2int(settle_bets) -> ?lim2 + 79;
atom2int(market) -> ?lim2 + 80;
atom2int(markets) -> ?lim2 + 81;
atom2int(oracles) -> ?lim2 + 82;
atom2int(market_data) -> ?lim2 + 83;
atom2int(learn_secret) -> ?lim2 + 84;
atom2int(give_block) -> ?lim2 + 85;
atom2int(channel_sync) -> ?lim2 + 86;
atom2int(locked_payment) -> ?lim2 + 87;
atom2int(peers) -> ?lim2 + 88;
atom2int(header) -> ?lim2 + 89;
atom2int(headers) -> ?lim2 + 90;
atom2int(channel_payment) -> ?lim2 + 91;
atom2int(emsg) -> ?lim2 + 92;
atom2int(new_channel) -> ?lim2 + 93;
atom2int(txs) -> ?lim2 + 94;
atom2int(ok) -> ?lim2 + 95;
atom2int(pow) -> ?lim2 + 96;
atom2int(prev_hashes) -> ?lim2 + 97;
atom2int(error) -> ?lim2 + 98;
atom2int(block) -> ?lim2 + 99;
atom2int(block_plus) -> ?lim2 + 100;
atom2int(ex) -> ?lim2 + 101;
atom2int(timeout) -> ?lim2 + 102;
atom2int(signed) -> ?lim2 + 103;
atom2int(bet) -> ?lim2 + 104;
atom2int(spk) -> ?lim2 + 105;
atom2int(ctc) -> ?lim2 + 106;
atom2int(ctc2) -> ?lim2 + 107;
atom2int(gc) -> ?lim2 + 108;
atom2int(delete_acc_tx) -> ?lim2 + 109;
atom2int(cs) -> ?lim2 + 110;
atom2int(nc) -> ?lim2 + 111;
atom2int(cr) -> ?lim2 + 112;
atom2int(spend) -> ?lim2 + 113;
atom2int(create_acc_tx) -> ?lim2 + 114;
atom2int(unmatched) -> ?lim2 + 115;
atom2int(oracle_bets) -> ?lim2 + 116;
atom2int(csc) -> ?lim2 + 117;
atom2int(repo) -> ?lim2 + 118;
atom2int(d) -> ?lim2 + 119;
atom2int(channel) -> ?lim2 + 120;
atom2int(gov) -> ?lim2 + 121;
atom2int(oracle) -> ?lim2 + 122;
atom2int(trees) -> ?lim2 + 123;
atom2int(trees2) -> ?lim2 + 124;
atom2int(trees3) -> ?lim2 + 125;
atom2int(trees4) -> ?lim2 + 126;
atom2int(trees5) -> ?lim2 + 127;
atom2int(share) -> ?lim2 + 128;
atom2int(acc) -> ?lim2 + 129;
atom2int(sub_acc) -> ?lim2 + 130;
atom2int(msg) -> ?lim2 + 131;
atom2int(cd) -> ?lim2 + 132;
atom2int(ob) -> ?lim2 + 133;
atom2int(order) -> ?lim2 + 134;
atom2int(orders) -> ?lim2 + 135;
atom2int(r) -> ?lim2 + 136;
atom2int(freq) -> ?lim2 + 137;
atom2int(sync) -> ?lim2 + 138;
atom2int(height) -> ?lim2 + 139;
atom2int(off) -> ?lim2 + 140;
atom2int(balance) -> ?lim2 + 141;
atom2int(mempool) -> ?lim2 + 142;
atom2int(top) -> ?lim2 + 143;
atom2int(sign) -> ?lim2 + 144;
atom2int(mine_block) -> ?lim2 + 145;
atom2int(add_peer) -> ?lim2 + 146;
atom2int(load_key) -> ?lim2 + 147;
atom2int(create_account) -> ?lim2 + 148;
atom2int(delete_account) -> ?lim2 + 149;
atom2int(account) -> ?lim2 + 150;
atom2int(sub_account) -> ?lim2 + 151;
atom2int(repo_account) -> ?lim2 + 152;
atom2int(channel_state) -> ?lim2 + 153;
atom2int(channel_balance) -> ?lim2 + 154;
atom2int(channel_balance2) -> ?lim2 + 155;
atom2int(channel_timeout) -> ?lim2 + 156;
atom2int(new_channel_with_server) -> ?lim2 + 157;
atom2int(pull_channel_state) -> ?lim2 + 158;
atom2int(add_secret) -> ?lim2 + 159;
atom2int(channel_spend) -> ?lim2 + 160;
atom2int(new_channel_tx) -> ?lim2 + 161;
atom2int(channel_solo_close) -> ?lim2 + 162;
atom2int(channel_team_close) -> ?lim2 + 163;
atom2int(channel_repo) -> ?lim2 + 164;
atom2int(channel_slash) -> ?lim2 + 165;
atom2int(channel_close) -> ?lim2 + 166;
atom2int(lightning_spend) -> ?lim2 + 167;
atom2int(new_difficulty_oracle) -> ?lim2 + 168;
atom2int(new_question_oracle) -> ?lim2 + 169;
atom2int(new_governance_oracle) -> ?lim2 + 170;
atom2int(oracle_new) -> ?lim2 + 171;
atom2int(oracle_bet) -> ?lim2 + 172;
atom2int(oracle_close) -> ?lim2 + 173;
atom2int(oracle_winnings) -> ?lim2 + 174;
atom2int(oracle_unmatched) -> ?lim2 + 175;
atom2int(pubkey) -> ?lim2 + 176;
atom2int(new_pubkey) -> ?lim2 + 177;
atom2int(channel_keys) -> ?lim2 + 178;
atom2int(market_match) -> ?lim2 + 179;
atom2int(new_market) -> ?lim2 + 180;
atom2int(trade) -> ?lim2 + 181;
atom2int(test_it_out) -> ?lim2 + 182;
atom2int(dump_channels) -> ?lim2 + 183;
atom2int(f) -> ?lim2 + 184;
atom2int(test) -> ?lim2 + 185;
atom2int(return) -> ?lim2 + 186;
atom2int(checkpoint) -> ?lim2 + 187;
atom2int(private) -> ?lim2 + 188;
atom2int(read_private) -> ?lim2 + 189;
atom2int(swap_tx) -> ?lim2 + 190;
atom2int(swap_tx2) -> ?lim2 + 191;
atom2int(swap_offer) -> ?lim2 + 192;
atom2int(swap_offer2) -> ?lim2 + 193;
atom2int(history) -> ?lim2 + 194;
atom2int(market_new_tx) -> ?lim2 + 195;
atom2int(market_liquidity_tx) -> ?lim2 + 196;
atom2int(market_swap_tx) -> ?lim2 + 197;
atom2int(trade_cancel_tx) -> ?lim2 + 198;
atom2int(buy_shares) -> ?lim2 + 199;
atom2int(combine_shares) -> ?lim2 + 200;
atom2int(inflate) -> ?lim2 + 201;
atom2int(nonce) -> ?lim2 + 202;
atom2int(stablecoin_new_tx) -> ?lim2 + 203;
atom2int(receipts) -> ?lim2 + 204;
atom2int(close_oracles) -> ?lim2 + 205;
atom2int(withdraw_from_oracles) -> ?lim2 + 206;
atom2int(tx) -> ?lim2 + 207;
atom2int(_) -> unknown.


int2atom(?lim2 + 0) -> true;
int2atom(?lim2 + 1) -> false;
int2atom(?lim2 + 2) -> contract;
int2atom(?lim2 + 3) -> contracts;
int2atom(?lim2 + 4) -> accounts;
int2atom(?lim2 + 5) -> sub_accounts;
int2atom(?lim2 + 6) -> sub_channels;
int2atom(?lim2 + 7) -> trades;
int2atom(?lim2 + 8) -> contract_new_tx;
int2atom(?lim2 + 9) -> contract_use_tx;
int2atom(?lim2 + 10) -> sub_spend_tx;
int2atom(?lim2 + 11) -> contract_evidence_tx;
int2atom(?lim2 + 12) -> contract_resolve_tx;
int2atom(?lim2 + 13) -> contract_timeout_tx;
int2atom(?lim2 + 14) -> contract_timeout_tx2;
int2atom(?lim2 + 15) -> contract_winnings_tx;
int2atom(?lim2 + 16) -> contract_simplify_tx;
int2atom(?lim2 + 17) -> pair_buy_tx;
int2atom(?lim2 + 18) -> pair_buy_offer;
int2atom(?lim2 + 19) -> team_buy_tx;
int2atom(?lim2 + 20) -> x;
int2atom(?lim2 + 21) -> final_spend;
int2atom(?lim2 + 22) -> waiver;
int2atom(?lim2 + 23) -> owner;
int2atom(?lim2 + 24) -> owner_layer;
int2atom(?lim2 + 25) -> tree;
int2atom(?lim2 + 26) -> sid;
int2atom(?lim2 + 27) -> sid_before;
int2atom(?lim2 + 28) -> before;
int2atom(?lim2 + 29) -> priority;
int2atom(?lim2 + 30) -> priority_before;
int2atom(?lim2 + 31) -> add;
int2atom(?lim2 + 32) -> take;
int2atom(?lim2 + 33) -> tx_scan;
int2atom(?lim2 + 34) -> version;
int2atom(?lim2 + 35) -> get_offer_contract;
int2atom(?lim2 + 36) -> get_offers;
int2atom(?lim2 + 37) -> oracle_list;
int2atom(?lim2 + 38) -> nc_offer;
int2atom(?lim2 + 39) -> channel_sig;
int2atom(?lim2 + 40) -> send;
int2atom(?lim2 + 41) -> read;
int2atom(?lim2 + 42) -> block_hash;
int2atom(?lim2 + 43) -> governance;
int2atom(?lim2 + 44) -> existence;
int2atom(?lim2 + 45) -> new_scalar_oracle;
int2atom(?lim2 + 46) -> scalar;
int2atom(?lim2 + 47) -> binary;
int2atom(?lim2 + 48) -> price;
int2atom(?lim2 + 49) -> lookup;
int2atom(?lim2 + 50) -> post;
int2atom(?lim2 + 51) -> ewah;
int2atom(?lim2 + 52) -> sync_normal;
int2atom(?lim2 + 53) -> sync_quick;
int2atom(?lim2 + 54) -> multi_tx;
int2atom(?lim2 + 55) -> time_value;
int2atom(?lim2 + 56) -> blocks;
int2atom(?lim2 + 57) -> work;
int2atom(?lim2 + 58) -> problem;
int2atom(?lim2 + 59) -> mining_data;
int2atom(?lim2 + 60) -> create_account_tx;
int2atom(?lim2 + 61) -> spend_tx;
int2atom(?lim2 + 62) -> combine_cancel_assets;
int2atom(?lim2 + 63) -> cancel_trade;
int2atom(?lim2 + 64) -> list_oracles;
int2atom(?lim2 + 65) -> keys_status;
int2atom(?lim2 + 66) -> keys_unlock;
int2atom(?lim2 + 67) -> keys_new;
int2atom(?lim2 + 68) -> halt;
int2atom(?lim2 + 69) -> ss;
int2atom(?lim2 + 70) -> exist;
int2atom(?lim2 + 71) -> key;
int2atom(?lim2 + 72) -> roots;
int2atom(?lim2 + 73) -> roots2;
int2atom(?lim2 + 74) -> roots3;
int2atom(?lim2 + 75) -> roots4;
int2atom(?lim2 + 76) -> roots5;
int2atom(?lim2 + 77) -> proof;
int2atom(?lim2 + 78) -> coinbase;
int2atom(?lim2 + 79) -> settle_bets;
int2atom(?lim2 + 80) -> market;
int2atom(?lim2 + 81) -> markets;
int2atom(?lim2 + 82) -> oracles;
int2atom(?lim2 + 83) -> market_data;
int2atom(?lim2 + 84) -> learn_secret;
int2atom(?lim2 + 85) -> give_block;
int2atom(?lim2 + 86) -> channel_sync;
int2atom(?lim2 + 87) -> locked_payment;
int2atom(?lim2 + 88) -> peers;
int2atom(?lim2 + 89) -> header;
int2atom(?lim2 + 90) -> headers;
int2atom(?lim2 + 91) -> channel_payment;
int2atom(?lim2 + 92) -> emsg;
int2atom(?lim2 + 93) -> new_channel;
int2atom(?lim2 + 94) -> txs;
int2atom(?lim2 + 95) -> ok;
int2atom(?lim2 + 96) -> pow;
int2atom(?lim2 + 97) -> prev_hashes;
int2atom(?lim2 + 98) -> error;
int2atom(?lim2 + 99) -> block;
int2atom(?lim2 + 100) -> block_plus;
int2atom(?lim2 + 101) -> ex;
int2atom(?lim2 + 102) -> timeout;
int2atom(?lim2 + 103) -> signed;
int2atom(?lim2 + 104) -> bet;
int2atom(?lim2 + 105) -> spk;
int2atom(?lim2 + 106) -> ctc;
int2atom(?lim2 + 107) -> ctc2;
int2atom(?lim2 + 108) -> gc;
int2atom(?lim2 + 109) -> delete_acc_tx;
int2atom(?lim2 + 110) -> cs;
int2atom(?lim2 + 111) -> nc;
int2atom(?lim2 + 112) -> cr;
int2atom(?lim2 + 113) -> spend;
int2atom(?lim2 + 114) -> create_acc_tx;
int2atom(?lim2 + 115) -> unmatched;
int2atom(?lim2 + 116) -> oracle_bets;
int2atom(?lim2 + 117) -> csc;
int2atom(?lim2 + 118) -> repo;
int2atom(?lim2 + 119) -> d;
int2atom(?lim2 + 120) -> channel;
int2atom(?lim2 + 121) -> gov;
int2atom(?lim2 + 122) -> oracle;
int2atom(?lim2 + 123) -> trees;
int2atom(?lim2 + 124) -> trees2;
int2atom(?lim2 + 125) -> trees3;
int2atom(?lim2 + 126) -> trees4;
int2atom(?lim2 + 127) -> trees5;
int2atom(?lim2 + 128) -> share;
int2atom(?lim2 + 129) -> acc;
int2atom(?lim2 + 130) -> sub_acc;
int2atom(?lim2 + 131) -> msg;
int2atom(?lim2 + 132) -> cd;
int2atom(?lim2 + 133) -> ob;
int2atom(?lim2 + 134) -> order;
int2atom(?lim2 + 135) -> orders;
int2atom(?lim2 + 136) -> r;
int2atom(?lim2 + 137) -> freq;
int2atom(?lim2 + 138) -> sync;
int2atom(?lim2 + 139) -> height;
int2atom(?lim2 + 140) -> off;
int2atom(?lim2 + 141) -> balance;
int2atom(?lim2 + 142) -> mempool;
int2atom(?lim2 + 143) -> top;
int2atom(?lim2 + 144) -> sign;
int2atom(?lim2 + 145) -> mine_block;
int2atom(?lim2 + 146) -> add_peer;
int2atom(?lim2 + 147) -> load_key;
int2atom(?lim2 + 148) -> create_account;
int2atom(?lim2 + 149) -> delete_account;
int2atom(?lim2 + 150) -> account;
int2atom(?lim2 + 151) -> sub_account;
int2atom(?lim2 + 152) -> repo_account;
int2atom(?lim2 + 153) -> channel_state;
int2atom(?lim2 + 154) -> channel_balance;
int2atom(?lim2 + 155) -> channel_balance2;
int2atom(?lim2 + 156) -> channel_timeout;
int2atom(?lim2 + 157) -> new_channel_with_server;
int2atom(?lim2 + 158) -> pull_channel_state;
int2atom(?lim2 + 159) -> add_secret;
int2atom(?lim2 + 160) -> channel_spend;
int2atom(?lim2 + 161) -> new_channel_tx;
int2atom(?lim2 + 162) -> channel_solo_close;
int2atom(?lim2 + 163) -> channel_team_close;
int2atom(?lim2 + 164) -> channel_repo;
int2atom(?lim2 + 165) -> channel_slash;
int2atom(?lim2 + 166) -> channel_close;
int2atom(?lim2 + 167) -> lightning_spend;
int2atom(?lim2 + 168) -> new_difficulty_oracle;
int2atom(?lim2 + 169) -> new_question_oracle;
int2atom(?lim2 + 170) -> new_governance_oracle;
int2atom(?lim2 + 171) -> oracle_new;
int2atom(?lim2 + 172) -> oracle_bet;
int2atom(?lim2 + 173) -> oracle_close;
int2atom(?lim2 + 174) -> oracle_winnings;
int2atom(?lim2 + 175) -> oracle_unmatched;
int2atom(?lim2 + 176) -> pubkey;
int2atom(?lim2 + 177) -> new_pubkey;
int2atom(?lim2 + 178) -> channel_keys;
int2atom(?lim2 + 179) -> market_match;
int2atom(?lim2 + 180) -> new_market;
int2atom(?lim2 + 181) -> trade;
int2atom(?lim2 + 182) -> test_it_out;
int2atom(?lim2 + 183) -> dump_channels;
int2atom(?lim2 + 184) -> f;
int2atom(?lim2 + 185) -> test;
int2atom(?lim2 + 186) -> return;
int2atom(?lim2 + 187) -> checkpoint;
int2atom(?lim2 + 188) -> private;
int2atom(?lim2 + 189) -> read_private;
int2atom(?lim2 + 190) -> swap_tx;
int2atom(?lim2 + 191) -> swap_tx2;
int2atom(?lim2 + 192) -> swap_offer;
int2atom(?lim2 + 193) -> swap_offer2;
int2atom(?lim2 + 194) -> history;
int2atom(?lim2 + 195) -> market_new_tx;
int2atom(?lim2 + 196) -> market_liquidity_tx;
int2atom(?lim2 + 197) -> market_swap_tx;
int2atom(?lim2 + 198) -> trade_cancel_tx;
int2atom(?lim2 + 199) -> buy_shares;
int2atom(?lim2 + 200) -> combine_shares;
int2atom(?lim2 + 201) -> inflate;
int2atom(?lim2 + 202) -> nonce;
int2atom(?lim2 + 203) -> stablecoin_new_tx;
int2atom(?lim2 + 204) -> receipts;
int2atom(?lim2 + 205) -> close_oracles;
int2atom(?lim2 + 206) -> withdraw_from_oracles;
int2atom(?lim2 + 207) -> tx;
int2atom(_) -> undefined_atom_int.
    

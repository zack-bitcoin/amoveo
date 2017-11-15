-module(testnet_hasher).
-export([doit/1, bin_to_hex/1, file/1]).

doit(X) -> hash:doit(X, constants:hash_size()).
bin_to_hex(<<>>) -> "";
bin_to_hex(<<A, B/binary>>) ->
    byte_to_hex(<<A>>) ++ bin_to_hex(B).
byte_to_hex(<< N1:4, N2:4 >>) ->
    [erlang:integer_to_list(N1, 16), erlang:integer_to_list(N2, 16)].
file(S) -> 
    {ok, F} = file:open(S, [read, binary]),
    O = file(F, <<>>, 0),
    ok = file:close(F),
    O.
file(F, O, S) ->
    B = 10000000,%need at least 10 megabytes of ram open
    case file:pread(F, B*S, B) of
	eof -> O;
	{ok, Out} -> file(F, doit(<<Out/binary, O/binary>>), S+1);
        {error, Reason} -> Reason
    end.

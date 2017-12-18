-module(checksum_address).
-export([]).
-define(cs, 8). %checksum size
checksum(X) -> checksum(0, X).
checksum(N, <<H:4, T/bitstring>>) ->
    checksum(N+H, <<T/bitstring>>);
checksum(N, <<>>) ->
    M = N rem 256,
    <<M:8>>.
-define(AddressEntropy, constants:address_entropy()).
pubkey2address(P) when size(P) > 66 ->
    pubkey2address(base64:decode(P));
pubkey2address(P) ->
    binary2address(hash:doit(P)).
address2binary(A) ->
    S = ?AddressEntropy,
    <<C:?cs, B:S>> = base58:base58_to_binary(binary_to_list((A))),
    <<C:?cs>> = checksum(<<B:S>>),
    <<B:S>>.
binary2address(B) -> 
    S = ?AddressEntropy,
    <<A:S>> = B,
    <<C:?cs>> = checksum(B),
    D = <<C:?cs, A:S>>,
    list_to_binary(base58:binary_to_base58(D)).
valid_address(A) ->
    AB = ?AddressEntropy,
    << C:?cs, B:AB >> = base58:base58_to_binary(binary_to_list(A)),
    D = checksum(<<B:AB>>),
    << C:?cs >> == D.

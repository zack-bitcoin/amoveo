-module(graphql_id).

-export([encode/1, decode/1]).

-spec encode({atom(), integer()}) -> binary().
%% tag::idEncode[]
encode({Tag, ID}) ->
    BinTag = atom_to_binary(Tag, utf8),
    IDStr = integer_to_binary(ID),
    base64:encode(<<BinTag/binary, ":", IDStr/binary>>).
%% end::idEncode[]

-spec decode(binary()) -> {error, Reason} | {ok, {atom(), integer()}}
  when
    Reason :: term().
%% tag::idDecode[]
decode(Input) ->
    try
        Decoded = base64:decode(Input),
        case binary:split(Decoded, <<":">>) of
            [BinTag, IDStr] ->
                {ok, {binary_to_existing_atom(BinTag, utf8),
                      binary_to_integer(IDStr)}};
            _ ->
                exit(invalid)
        end
    catch
        _:_ ->
            {error, invalid_decode}
    end.
%% end::idDecode[]

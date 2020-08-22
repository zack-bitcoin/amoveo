-module(contract_new_tx).
-export([go/4, make_dict/6, make_dict/4]).
-include("../../records.hrl").
make_dict(From, CH, Types, Fee) ->
    make_dict(From, CH, Types, <<0:256>>, 0, Fee).
make_dict(From, CH, Types, Source, SourceType, Fee) ->
    #contract_new_tx{from = From, fee = Fee, contract_hash = CH, many_types = Types, source = Source, source_type = SourceType}.

go(Tx, Dict, NewHeight, _) ->
    true = NewHeight > forks:get(32),
    #contract_new_tx{
    from = From,
    fee = Fee,
    contract_hash = CH,
    source = Source,
    source_type = SourceType,
    many_types = MT} = Tx,
    MCF = governance:dict_get_value(max_contract_flavors, Dict),
    true = MCF >= MT,
    Facc = accounts:dict_update(From, Dict, -Fee, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    Key = contracts:make_id(CH, MT, Source, SourceType),
    empty = contracts:dict_get(Key, Dict2),
    NC = contracts:new(CH, MT, Source, SourceType),
    contracts:dict_write(NC, Dict2).
    
    

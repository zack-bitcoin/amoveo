-module(resolve_contract_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").

make_dict(From, Contract, Evidence, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    #resolve_contract_tx{from = From, nonce = Nonce, fee = Fee, contract = Contract, evidence = Evidence}.
    
go(Tx, Dict, NewHeight, _) ->
    #resolve_contract_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    contract = ContractBytecode,
    evidence = Evidence
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    CID = hash:doit(ContractBytecode),
    Contract = contracts:dict_get(CID, Dict2),
    #contract{
      many_types = Many,
      nonce = ContractNonce,
      last_modified = LM,
      delay = Delay,
      closed = 0,
      result = Result,
      source = Source,
      source_type = SourceType,
      volume = Volume
     } = Contract,
    Funs = governance:dict_get_value(fun_limit, Dict2),
    Vars = governance:dict_get_value(var_limit, Dict2),
    OpGas = governance:dict_get_value(time_gas, Dict2),
    RamGas = governance:dict_get_value(space_gas, Dict2),
    State = chalang:new_state(NewHeight, 0, 0),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, <<>>, ContractBytecode, State, constants:hash_size(), 2, false),
    case chalang:run5(ContractBytecode, Data) of
        {error, Error} ->
            io:fwrite("in resolve_contract_tx, contract has an error\n"),
            Dict2;
        Data2 ->
            [Result,Nonce,Delay|_] = chalang:stack(Data2),
            true = Nonce > ContractNonce,
            Contract2 = Contract#contract{
                          result = <<Result:256>>,
                          nonce = Nonce,
                          delay = Delay,
                          last_modified = NewHeight
                         },
            contracts:dict_write(Contract2, Dict2)
    end.
    

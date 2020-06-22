-module(resolve_contract_tx).
-export([go/4, make_dict/6]).
-include("../../records.hrl").

make_dict(From, Contract, CID, Evidence, Prove, Fee) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    #resolve_contract_tx{from = From, nonce = Nonce, fee = Fee, contract = Contract, evidence = Evidence, prove = Prove, contract_id = CID}.
    
go(Tx, Dict, NewHeight, _) ->
    #resolve_contract_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    contract = ContractBytecode,
    contract_id = CID,
    evidence = Evidence,
    prove = Prove
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
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
    CH = hash:doit(ContractBytecode),
    CID = contracts:make_id({CH, Many}),%verify that this is the correct code for this contract.

    Funs = governance:dict_get_value(fun_limit, Dict2),
    Vars = governance:dict_get_value(var_limit, Dict2),
    OpGas = governance:dict_get_value(time_gas, Dict2),
    RamGas = governance:dict_get_value(space_gas, Dict2),
    State = chalang:new_state(NewHeight, 0, 0),
    ProveCode = spk:prove_facts(Prove, Dict2, NewHeight),
    true = chalang:none_of(Evidence),
    AllCode = <<Evidence/binary, ProveCode/binary, ContractBytecode/binary>>,
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, <<>>, AllCode, State, constants:hash_size(), 2, false),
    case chalang:run5(AllCode, Data) of
        {error, Error} ->
            io:fwrite("in resolve_contract_tx, contract has an error\n"),
            Dict2;
        Data2 ->
            [<<CNonce:32>>,<<CDelay:32>>,<<CResult:256>>|_] = chalang:stack(Data2),
            true = CNonce > ContractNonce,
            Contract2 = Contract#contract{
                          result = <<CResult:256>>,
                          nonce = CNonce,
                          delay = CDelay,
                          last_modified = NewHeight
                         },
            contracts:dict_write(Contract2, Dict2)
    end.
    

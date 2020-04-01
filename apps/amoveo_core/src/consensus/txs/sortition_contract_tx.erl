-module(sortition_contract_tx).
-export([go/4, make_dict/6]).

-include("../../records.hrl").
make_dict(From, Fee, SID, LN, Contract, Proof) ->
    Acc = trees:get(accounts, From),
    #sortition_contract_tx{pubkey = From, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, contract = Contract, proof = Proof, layer = LN}.
    
go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_contract_tx{
    pubkey = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    contract = Contract, 
    proof = Proof,
    layer = LN
   } = Tx,
    F28 = forks:get(28),
    true = NewHeight > F28,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(A2, Dict),
    S = sortition:dict_get(SID, Dict2),
    #sortition{
                top_candidate = TCID_0
              } = S,
    TCID = sortition_claim_tx:layer_salt(TCID_0, LN),
    TC = candidates:dict_get(TCID, Dict2),
    #candidate{
                next_candidate = NC,
                contracts_root = HashesRoot
              } = TC,
    S2 = S#sortition{
           top_candidate = NC,
           last_modified = NewHeight
          },
    Dict3 = sortition:dict_write(S2, Dict2),
    Funs = governance:dict_get_value(fun_limit, Dict3),
    Vars = governance:dict_get_value(var_limit, Dict3),
    OpGas = governance:dict_get_value(time_gas, Dict3),
    RamGas = governance:dict_get_value(space_gas, Dict3),
    State = chalang:new_state(NewHeight, 0, 0),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, <<>>, Contract, State, constants:hash_size(), 2, false),
    ContractHash = hash:doit(Contract),
    case chalang:run5(Contract, Data) of
        {error, Error} ->
            io:fwrite("in sortition contract tx, smart contract has an error\n"),
            Dict2;
        Data2 ->
            ContractHash2 = 
                case chalang:stack(Data2) of
                    [<<1:32>>|_] -> ContractHash;
                    _ -> ownership:contract_flip(ContractHash)
                        
                end,
            Size = 32,
            KeyLength = 2,
            M = mtree:new_empty(KeyLength, Size, 0),
            CFG = mtree:cfg(M),
            Leaf = leaf:new(0, ContractHash2, 0, CFG),
            ProofCheck = verify:proof(HashesRoot, Leaf, Proof, CFG),
            if
                ProofCheck -> Dict3;
                true -> Dict2
            end
    end.


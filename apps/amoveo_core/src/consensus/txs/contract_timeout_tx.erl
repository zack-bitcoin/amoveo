-module(contract_timeout_tx).
-export([go/4, make_dict/4, make_dict/3]).
-include("../../records.hrl").

make_dict(From, ContractID, Fee) ->
    make_dict(From, ContractID, Fee, 0).
make_dict(From, ContractID, Fee, Proof) ->
    A = trees:get(accounts, From),
    Nonce = A#acc.nonce + 1,
    #contract_timeout_tx{from = From, nonce = Nonce, fee = Fee, contract_id = ContractID, proof = Proof}.

go(Tx, Dict, NewHeight, _) ->
    #contract_timeout_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    contract_id = CID,
    proof = Proof
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    Contract = contracts:dict_get(CID, Dict2),
    #contract{
               last_modified = LM,
               delay = Delay,
               resolve_to_source = RTS,
               source = Source,
               source_type = SourceType,
               sink = CID2,
               %result = Result,
               closed = 0
   } = Contract,
    true = (LM + Delay) < NewHeight,
    Contract2 = 
        Contract#contract{
          closed = 1
         },
    case RTS of
        1 ->
            contracts:dict_write(Contract2, Dict2);
        0 ->
            %move money to another contract
            V = Contract2#contract.volume,
            Contract3 = Contract2#contract{
                          volume = 0
                         },
            MT = mtree:new_empty(5, 32, 0),
            CFG = mtree:cfg(MT),
            {_,
             _, 
             {Result, CID2, Proof2}} = Proof,
            CH2Leaf = leaf:new(0, CID2, 0, CFG),
            true = verify:proof(Result, CH2Leaf, Proof2, CFG),
            %CH2 = leaf:value(CH2Leaf),
            Contract4_0 = contracts:dict_get(CID2, Dict2),
            Contract4 = 
                case Contract4_0 of
                    empty -> 
                        {{Row, CH2},
                         {Result, RowHash, Proof1}, 
                         _} = Proof,
                        RowLeaf = leaf:new(1, RowHash, 0, CFG),
                        true = verify:proof(Result, RowLeaf, Proof1, CFG),
                        RowHash = leaf:value(RowLeaf),
                        RowHash = hash:doit(resolve_contract_tx:serialize_row(Row, <<>>)),
                        RMany = length(Row),
                        CID2 = contracts:make_id(CH2, RMany, Source, SourceType),%this is to verify that CH2 is correct.
                        contracts:new(CH2, RMany, Source, SourceType);
                    X -> X
                end,
            V2 = Contract4#contract.volume,
            Contract5 = Contract4#contract{
                          volume = V + V2
                         },
            Dict3 = contracts:dict_write(Contract5, Dict2),
            contracts:dict_write(Contract3, Dict3)
    end.

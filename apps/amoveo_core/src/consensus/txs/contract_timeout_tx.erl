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
               result = Result,
               closed = 0
   } = Contract,
    true = (LM + Delay) < NewHeight,
    Contract2 = 
        Contract#contract{
          closed = 1
         },
    case RTS of
        0 ->
            %move money to another contract
            V = Contract2#contract.volume,
            Contract3 = Contract2#contract{
                          volume = 0
                         },
            MT = mtree:new_empty(5, 32, 0),
            CFG = mtree:cfg(MT),
            {Row,
             {Result, RowLeaf, Proof1}, 
             {Result, CH2Leaf, Proof2}} = Proof,
            true = verify:proof(Result, RowLeaf, Proof1, CFG),
            true = verify:proof(Result, CH2Leaf, Proof2, CFG),
            RowHash = leaf:value(RowLeaf),
            RowHash = hash:doit(resolve_contract_tx:serialize_row(Row, <<>>)),
            CH2 = leaf:value(CH2Leaf),
            RMany = length(Row),
            CID2 = contracts:make_id(CH2, RMany, Source, SourceType),
            Contract4 = contracts:dict_get(CID2, Dict2),
            io:fwrite("contract timeout tx, contract4 "),
            io:fwrite(packer:pack(Contract4)),
            io:fwrite("\n"),
            
            V2 = Contract4#contract.volume,
            Contract5 = Contract4#contract{
                          volume = V + V2
                         },
            Dict3 = contracts:dict_write(Contract5, Dict2),
            contracts:dict_write(Contract3, Dict3);
        1 ->
            contracts:dict_write(Contract2, Dict2)
    end.

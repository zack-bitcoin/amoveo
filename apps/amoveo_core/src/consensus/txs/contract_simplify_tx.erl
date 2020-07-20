-module(contract_simplify_tx).
-export([go/4, make_dict/7, 
         apply_matrix2vector/2,
         apply_matrix2matrix/2]).
-include("../../records.hrl").


make_dict(Pubkey, CID, CID2, CID3, Matrix, PayoutVector, Fee) ->
    A = trees:get(accounts, Pubkey),
    Nonce = A#acc.nonce + 1,
    #contract_simplify_tx{from = Pubkey, nonce = Nonce, cid = CID, cid2 = CID2, cid3 = CID3, m1 = Matrix, m2 = PayoutVector, fee = Fee}.

go(Tx, Dict, NewHeight, _) ->
    #contract_simplify_tx{
    from = From, 
    nonce = Nonce, 
    cid = CID, 
    cid2 = CID2,
    cid3 = CID3,
    m1 = Matrix, 
    m2 = Matrix2, 
    fee = Fee
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    Contract1 = contracts:dict_get(CID, Dict2),
    #contract{
               result = MerkelHash,
               closed = 1,
               source = Source,
               source_type = SourceType,
               sink = Sink
             } = Contract1,
    false = Sink == <<0:256>>,
    %Row = hd(Matrix),
    {MRoot, M2} = resolve_contract_tx:make_tree(Matrix),
    MerkelHash = mtree:root_hash(MRoot, M2),
    %CID2 = contracts:make_id(CH, length(Row), Source, SourceType),
    Contract2 = contracts:dict_get(CID2, Dict2),
    #contract{
               result = MerkleHash2,
               closed = 1,
               source = Source,
               source_type = SourceType,
               sink = Sink2
             } = Contract2,
    case Sink2 of
        <<0:256>> ->
            PayoutVector = Matrix2,
            MerkleHash2 = hash:doit(resolve_contract_tx:serialize_row(PayoutVector, <<>>)),

            NewPayoutVector = apply_matrix2vector(Matrix, PayoutVector),
            NewMerkleHash = hash:doit(resolve_contract_tx:serialize_row(NewPayoutVector, <<>>)),
            Contract3 = Contract1#contract{
                          result = NewMerkleHash
                         },
            contracts:dict_write(Contract3, Dict2);
        CID3 ->
            {MRoot2, M3} = resolve_contract_tx:make_tree(Matrix2),
            MerkelHash2 = mtree:root_hash(MRoot2, M3),
            Contract3 = contracts:dict_get(CID3, Dict2),
            #contract{
                       closed = 0
            %if contract2 resolves to another contract, we need to verify that that contract is not yet closed.
            %this way you are forced to simplify starting at the more efficient end.
                     } = Contract3,

            Matrix3 = apply_matrix2matrix(Matrix, Matrix2), 
            {MRoot3, M4} = resolve_contract_tx:make_tree(Matrix3),
            NewMerkleHash = mtree:root_hash(MRoot3, M4),
            Contract4 = Contract1#contract{
                          result = NewMerkleHash,
                          sink = CID3
                         },
            contracts:dict_write(Contract4, Dict2)
    end.


apply_matrix2vector([], _) -> [];
apply_matrix2vector([H|T], V) -> 
    [dot(H, V)|
     apply_matrix2vector(T, V)].
max() ->
    <<M:32>> = <<-1:32>>,
    M.
dot(A, B) ->
    dot(A, B, 0).
dot([], [], N) -> 
    X = N div max(),
    <<X:32>>;
dot([<<H1:32>>|T1],[<<H2:32>>|T2],N) -> 
    dot(T1, T2, N + (H1 * H2)).

apply_matrix2matrix(M1, M2) ->
    M3 = flip(M2),
    am2m(M1, M3).
am2m([], _) -> [];
am2m([H|T], M2) -> 
    [lists:map(fun(M) -> dot(H, M) end, M2)|
     am2m(T, M2)].
                      
flip([[]|_]) -> [];
flip(L) -> 
    F = lists:map(fun(X) -> hd(X) end, L),
    T = lists:map(fun(X) -> tl(X) end, L),
    [F|flip(T)].

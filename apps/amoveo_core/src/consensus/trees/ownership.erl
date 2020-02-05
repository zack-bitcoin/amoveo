-module(ownership).
-export([new/5,
         make_tree/1,
         make_proof/2,

         pubkey/1,
         pstart/1,
         pend/1,
         contract/1,
         sid/1,
         
         verify/2,

         serialize/1,
         deserialize/1,

         test/0
        ]).

%we could have a binary merkel tree, where every step of the tree says how the probabilistic value space is divided between the two child branches. Then walking down a path could give you certainty that there is no overlap.
%This option would probably have the least computation and bandwidth requirements.

%We want the same team of validators to be able to manage multiple sortition chains, and use a single merkel root to commit to updates on the different sortition chains simultaneously.

%we also want the ability to assign the same probability space to different people, at the same block height, as long as they take opposite sides of a smart contract.


% There is this board game called "Guess Who?"
%Where there are all these little pictures of people. Your opponent picks one of the pictures. 
%and you ask yes/no questions to try and narrow down which of the people they had chosen.

%So the strategy is to try and ask a question such that 1/2 the people would be "yes", and 1/2 would be "no". that way, no matter what the answer is, you can eliminate 1/2 the suspects.

%building up these merkle trees is going to be similar.

%In order to minimize the length of any individual merkel proof, we want the tree to be balanced.
%To make a balanced tree, we need to keep choosing questions such that 1/2 of the elements we need to put in the tree are "yes", and 1/2 are "no".

-record(owner, {pubkey, %pubkey of who owns this probabilistic value space.
            pstart, %start of the probability space
            pend, %end of the probability space
            sortition_id,
            contract}).
-record(tree, {rule, 
               b1, h1, b0, h0}).
new(P, S, E, C, SID) ->
    32 = size(SID),
    32 = size(S),
    32 = size(E),
    #owner{pubkey = P,
       pstart = S,
       pend = E,
       sortition_id = SID,
       contract = hash:doit(C)}.
pubkey(X) -> X#owner.pubkey.
pstart(X) -> X#owner.pstart.
pend(X) -> X#owner.pend.
contract(X) -> X#owner.contract.
sid(X) -> X#owner.sortition_id.

make_tree(Owners) ->
    Owners2 = lists:sort(
                fun(A, B) ->
                        <<A1:256>> = A#owner.sortition_id,
                        <<B1:256>> = B#owner.sortition_id,
                        A1 =< B1 end, Owners),
    ListsOwners = 
        make_lists(fun(X) -> X#owner.sortition_id end, 
                   Owners2),%there are sub-lists each with a unique SID.
    Tree1 = make_tree_sid2(ListsOwners),
    add_hashes(Tree1).
add_hashes(X) when is_record(X, tree) ->
    #tree{
           b0 = B0,
           b1 = B1
         } = X,
    {H0, B02} = add_hashes(B0),
    {H1, B12} = add_hashes(B1),
    X2 = X#tree{
           b0 = B02,
           h0 = H0,
           b1 = B12,
           h1 = H1
          },
    H2 = hash:doit(serialize_tree(X2)),
    {H2, X2};
add_hashes(X) when is_record(X, owner)->
    {hash:doit(serialize(X)), X}.
make_tree_sid2([A]) -> make_tree_prob(A);
make_tree_sid2(ListsOwners) ->
    L = length(ListsOwners),
    L2 = L div 2,
    OwnerNth = hd(lists:nth(L2, ListsOwners)), 
    SID = OwnerNth#owner.sortition_id,
    {LA, LB} = lists:split(L2, ListsOwners),
    false = [] == LA,
    false = [] == LB,
    #tree{rule = {sid_before, SID},
          b1 = make_tree_sid2(LA),
          b0 = make_tree_sid2(LB)}.
make_tree_prob(Owners) ->
    Owners2 = lists:sort(
                fun(A, B) ->
                        <<A1:256>> = A#owner.pstart,
                        <<B1:256>> = B#owner.pstart,
                        A1 =< B1 end, Owners),
    no_overlap_check(Owners2),
    make_tree_prob2(Owners2).
make_tree_prob2([A]) -> A;
make_tree_prob2(ListsOwners)->
    L = length(ListsOwners),
    L2 = L div 2,
    OwnerNth = lists:nth(L2, ListsOwners),
    Owner = OwnerNth,
    PE = Owner#owner.pend,
    {LA, LB} = lists:split(L2, ListsOwners),
    #tree{rule = {before, PE},
          b1 = make_tree_prob2(LA),
          b0 = make_tree_prob2(LB)}.
                                 
no_overlap_check([]) -> ok;
no_overlap_check([_]) -> ok;
no_overlap_check([A|[B|T]]) -> 
    true = A#owner.contract == B#owner.contract,
    true = A#owner.pend =< B#owner.pstart,
    no_overlap_check([B|T]).
            
make_lists(F, [H|T]) ->
    lists:reverse(make_lists2(F, F(H), T, [H], [])).
make_lists2(_F, _TID, [], NL, R) -> 
    [lists:reverse(NL)|R];
make_lists2(F, TID, [N|IL], NL, R) ->
    TID2 = F(N),
    if
        TID == TID2 -> 
            make_lists2(F, TID, IL, [N|NL], R);
        true ->
            make_lists2(F, TID2, IL, [N], [lists:reverse(NL)|R])
    end.
            

make_proof(Owner, Owner) -> [];
make_proof(Owner, Tree) when is_record(Tree, tree) ->
    %grab all the elements leading towards Owner.
    #tree{
           b1 = Branch1,
           b0 = Branch0
         } = Tree,
    Direction = contract_direction(Tree, Owner),
    B = if
            Direction -> Branch1;
            true -> Branch0
        end,
    T2 = Tree#tree{b1 = 0, b0 = 0},
    [T2|make_proof(Owner, B)];
make_proof(A, B) ->
    io:fwrite({A, B}).


verify(Ownership, Proof) ->
    %returns a root hash, so we can check that the Proof is linked to something else.
    X = hash:doit(serialize(Ownership)),
    verify2(Ownership, X, Proof).%starts from leaf, works towards root.
verify2(_, Root, []) -> Root;
verify2(Ownership, Root, [H|T]) ->
    #tree{
           h1 = H1,
           h0 = H0
         } = H,
    Result = contract_direction(H, Ownership),%run_contract(H, Ownership, Dict),
    Root = if
               Result -> H1;
               true -> H0
           end,
    NewRoot = hash:doit(serialize_tree(H)),
    verify2(Ownership, NewRoot, T).


contract_direction(Tree, Owner) ->
    #owner{
            sortition_id = SID,
            pstart = <<PStart:256>>,%this moves too.
            pend = <<PEnd:256>>,
            contract = CH1
        } = Owner,
    true = PStart < PEnd,%TODO, move this where it belongs. 
    #tree{
           rule = Contract
         } = Tree,
    case Contract of
        {sid_before, <<SID2:256>>} -> 
            <<SID1:256>> = SID,
            SID1 =< SID2;
        {sid, SID2} -> 
            SID == SID2;
        {before, <<N:256>>} -> 
            if
                (PEnd =< N) -> true;
                true ->
                    true = PStart >= N,
                    false
            end;
        %PEnd =< N;
        {contract, CH1} -> 
            true;
        {contract, <<CH2:256>>} -> 
            <<CH:256>> = CH1,
            true = (CH + CH2) == 0,
            false
    end.
            
    

get_merkel_facts(Evidence, Dict) ->
    ok.
run_contract(Tree, Ownership, Evidence, Dict) ->
    #owner{
        sortition_id = SID,
        pstart = PStart,
        pend = PEnd
        } = Ownership,
    #tree{
           rule = Contract
         } = Tree,
    Facts = get_merkel_facts(Evidence, Dict),
    OpGas = 10000,
    RamGas = 10000,
    Vars = 1000,
    Funs = 1000,
    State = chalang:new_state(99999999, 0, 0),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, Evidence, Contract, State, constants:hash_size(), 2, false),
    Data2 = chalang:run5(Facts, Data),
    Data3 = chalang:run5(Contract, Data2),
    hd(chalang:stack(Data3)).


    
serialize_tree(T) ->
    #tree{
           rule = {Type, C},
           h0 = H0,
           h1 = H1
         } = T,
    A = case Type of
            sid -> 1;
            before -> 2;
            contract -> 3;
            sid_before -> 4
        end,
                   
    <<C/binary, H0/binary, H1/binary, A:8>>.

serialize(X) ->
    PS = constants:pubkey_size(),
    HS = constants:hash_size(),
    #owner{
        pubkey = P,
        pstart = S,
        pend = E,
        sortition_id = SID,
        contract = C
      } = X,
    PS = size(P),
    32 = size(S),
    32 = size(E),
    HS = size(C),
    HS = size(SID),
    <<P/binary,
      S/binary,
      E/binary,
      SID/binary,
      C/binary>>.
deserialize(B) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size()*8,
    X = 32*8,
    <<
      P:PS,
      S:X,
      E:X,
      SID:HS,
      C:HS
    >> = B,
    #owner{
        pubkey = <<P:PS>>,
        pstart = <<S:X>>,
        pend = <<E:X>>,
        sortition_id = <<SID:HS>>,
        contract = <<C:HS>>
      }.

test() ->
    SID = hash:doit(1),
    SID2 = hash:doit(2),
    <<Max:256>> = <<-1:256>>,
    M1 = Max div 6,
    M2 = M1 + M1,
    M3 = M2 + M1,
    M4 = M2 + M2,
    M5 = M2 + M3,
    M6 = Max - 1,
    X1 = new(keys:pubkey(),
             <<0:256>>,
             <<M3:256>>,
             <<>>,
             SID),
    X2 = new(keys:pubkey(),
             <<M3:256>>,
             <<M4:256>>,
             <<>>,
             SID),
    X3 = new(keys:pubkey(),
             <<M4:256>>,
             <<M5:256>>,
             <<>>,
             SID),
    X4 = new(keys:pubkey(),
             <<M5:256>>,
             <<M6:256>>,
             <<>>,
             SID),
    X5 = new(keys:pubkey(),
             <<0:256>>,
             <<M6:256>>,
             <<>>,
             SID2),
    L1 = [X1, X2, X5],
    L2 = [X1, X2, X3, X4, X5],
    {Root, T} = make_tree(L2),
    Proof = lists:reverse(make_proof(X1, T)),
    Root = verify(X1, Proof),
    %{X1, Proof}.
    success.
    

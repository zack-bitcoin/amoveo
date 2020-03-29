-module(ownership).
-export([new/7,
         make_tree/1,
         make_proof/2,

         pubkey/1,
         pubkey2/1,
         pstart/1,
         pend/1,
         contracts/1,
         priority/1,
         sid/1,
         
         verify/2,
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
                pubkey2, %if it is a channel, then we need 2 pubkeys.
                pstart, %start of the probability space
                pend, %end of the probability space
                priority,
                sortition_id,
                contracts = []
               }).
-record(bounds, {contracts = [],
                 pstart = <<0:256>>,
                 pend = <<-1:256>>
                }).
-record(tree, {rule, 
               b1, h1, b0, h0}).
new(P, P2, S, E, Pr, SID, Contracts) ->
    32 = size(SID),
    32 = size(S),
    32 = size(E),
    #owner{pubkey = P,
           pubkey2 = P2,
           pstart = S,
           pend = E,
           sortition_id = SID,
           priority = Pr,
           contracts = Contracts
           %contract = hash:doit(C)
          }.
pubkey(X) -> X#owner.pubkey.
pubkey2(X) -> X#owner.pubkey2.
pstart(X) -> X#owner.pstart.
pend(X) -> X#owner.pend.
contracts(X) -> X#owner.contracts.
sid(X) -> X#owner.sortition_id.
priority(X) -> X#owner.priority.

make_tree(Owners) ->
    Owners2 = 
        lists:sort(
          fun(A, B) ->
                  A1 = A#owner.priority,
                  B1 = B#owner.priority,
                  A1 =< B1 end, Owners),
    ListsOwners = 
        make_lists(fun(X) -> X#owner.priority end, 
                   Owners2),%there are sub-lists each with a unique priority.

    Tree1 = make_tree_priority2(ListsOwners),

    add_hashes(Tree1).
make_tree_priority2([A]) -> make_tree_sid(A);
make_tree_priority2(ListsOwners) -> 
    L = length(ListsOwners),
    L2 = L div 2,
    OwnerNth = hd(lists:nth(L2, ListsOwners)), 
    P = OwnerNth#owner.priority,
    {LA, LB} = lists:split(L2, ListsOwners),
    false = [] == LA,
    false = [] == LB,
    #tree{rule = {priority_before, P},
          b1 = make_tree_priority2(LA),
          b0 = make_tree_priority2(LB)}.

    
make_tree_sid(Owners) ->
    Owners2 = lists:sort(
                fun(A, B) ->
                        <<A1:256>> = A#owner.sortition_id,
                        <<B1:256>> = B#owner.sortition_id,
                        A1 =< B1 end, Owners),
    ListsOwners = 
        make_lists(fun(X) -> X#owner.sortition_id end, 
                   Owners2),%there are sub-lists each with a unique SID.
    make_tree_sid2(ListsOwners).
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
    %if people are using smart contracts so that their probability space overlaps with more than one other person's probability space, then divide their ownership objects such that each object is either 100% overlapping with all the others in the same probability space, or else it is 0% overlapping. no partial overlap.
    %make sub-lists of ownership objects that overlap the same probability space, we will divide up contract space in the next step.
    Owners2 = 
        lists:sort(
          fun(A, B) ->
                  <<A1:256>> = A#owner.pstart,
                  <<B1:256>> = B#owner.pstart,
                  A1 =< B1 end, Owners),
    Owners3 = prob_sublists(Owners2),
    %no_overlap_check(Owners2),
    Bounds = #bounds{},
    make_tree_prob2(Owners3, Bounds).
make_tree_prob2([A], Bounds) -> 
    make_tree_contracts(A, Bounds);
%make_tree_prob2([[]|T], Bounds)->
%    make_tree_prob2(T, Bounds);
make_tree_prob2(ListsOwners, Bounds)->
    L = length(ListsOwners),
    L2 = L div 2,
    Owner = hd(lists:nth(L2, ListsOwners)),
    PE = Owner#owner.pend,
    {LA, LB} = lists:split(L2, ListsOwners),
    Rule = {before, PE},
    #tree{rule = Rule,
          b1 = make_tree_prob2(LA, bounds_update(Rule, Bounds)),
          b0 = make_tree_prob2(LB, bounds_update2(Rule, Bounds))}.
prob_sublists(L) ->
    %break the list into sublists where they probability space for each sublist is 100% overlapping.
    %when necessary, cut ownership contracts into 2 smaller ones.

    Ps = lists:map(fun(X) -> X#owner.pend end, L) ++ lists:map(fun(X) -> X#owner.pstart end, L),
    Ps2 = lists:sort(
            fun(A, B) -> A =< B end,
            Ps),
    Ps3 = remove_repeats(Ps2),
    prob_sublists2(Ps3, L, []).
prob_sublists2([_], _, X) -> lists:reverse(X);
prob_sublists2([A|[B|T]], L, X) ->
    
    %walk forward through the intervals defined by the points in Ps, for each interval find all the ownership contracts for this interval, and chop them up to fit in the interval if needed.

    %L2, remove the completed portion  of the prob space from the leading contracts which may contain it.
    {L2, Batch} = prob_sublists3(A, B, L, [], []),
    %X2, add a list of all the ownership contracts that used this portion of the prob space, chop up contracts as needed.
    if
        Batch == [] ->
            prob_sublists2([B|T], L2, X);
        true ->
            prob_sublists2([B|T], L2, [Batch|X])
    end.
    
prob_sublists3(S, E, [], First, Batch) ->
    {lists:reverse(First), Batch};
prob_sublists3(S, E, [CH|CT], First, Batch) ->
    HOS = CH#owner.pstart,
    HOPE = CH#owner.pend,
    <<EV:256>> = E,
    <<HOSV:256>> = HOS,
    %<<HOPEV:256>> = HOPE,
    if
        EV =< HOSV ->%next contract is outside of the interval we are currently looking at.
            {lists:reverse(First) ++ [CH|CT], 
             Batch};
        E == HOPE ->%next contract matches the interval we are looking at.
            prob_sublists3(
              S, E, CT, First, [CH|Batch]);
        true -> %need to chop
            CA = CH#owner{pend = E},
            CB = CH#owner{pstart = E},
            prob_sublists3(
              S, E, CT, 
              [CA|First], 
              [CB|Batch])
    end.
make_tree_contracts(L, Bounds) -> 
    %first get a list of all contract hashes and their inverses that are used for this list.
    CH1 = lists:map(
            fun(X) -> X#owner.contracts end,
            L),
    CH2 = lists:foldr(
            fun(A, B) -> A ++ B end,
            [],
            CH1),
    CH3 = lists:sort(
            fun(<<A:256>>, <<B:256>>) ->
                    A =< B
            end, CH2),
    CH4 = remove_repeats(CH3),
    CH5 = pair_inverses(CH4),
    make_tree_contracts2(L, Bounds, CH5).
make_tree_contracts2([A], Bounds, _) ->            
    B = in_bounds(A, Bounds),
    if 
        B -> A;
        true ->
            io:fwrite("in bounds failure\n"),
            io:fwrite(packer:pack({A, Bounds})),
            io:fwrite("\n"),
            1=2
    end;
make_tree_contracts2(X, Bounds, []) ->
    io:fwrite("in bounds failure 2\n"),
    io:fwrite(packer:pack({X, Bounds})),
    io:fwrite("\n"),
    1=2;
make_tree_contracts2(L, Bounds, [{H1, H2}|Pairs]) ->
    {LA, LB} = lists:split_with(
                 fun(X) ->
                         is_in(H1, X#owner.contracts) end,
                 L),
    {_, []} = %everything needs to be on one side or the other. sanity check.
        lists:split_with(
          fun(X) ->
                  is_in(H2, X#owner.contracts) end,
          LB),
    Rule = {contract, H1},
    #tree{rule = Rule,
          b1 = make_tree_contracts2(LA, bounds_update(Rule, Bounds), Pairs),
          b0 = make_tree_contracts2(LB, bounds_update2(Rule, Bounds), Pairs)}.
    
       
pair_inverses([]) -> [];
pair_inverses([H|T]) -> 
    H2 = contract_flip(H),
    T2 = remove_element(H2, T),
    [{H, H2}|pair_inverses(T2)].

remove_element(_, []) -> [];
remove_element(A, [A|T]) -> T;
remove_element(A, [B|T]) -> 
    [B|remove_element(A, T)].

remove_repeats([]) -> [];
remove_repeats([A]) -> [A];
remove_repeats([A|[A|T]]) ->
    remove_repeats([A|T]);
remove_repeats([H|T]) -> 
    [H|remove_repeats(T)].
                              
                            

%no_overlap_check([]) -> ok;
%no_overlap_check([_]) -> ok;
%no_overlap_check([A|[B|T]]) -> 
%    true = A#owner.pend =< B#owner.pstart,
%    no_overlap_check([B|T]).
            
make_lists(F, []) -> [];
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
    Bounds = #bounds{},
    verify2(Ownership, X, Proof, Bounds).%starts from leaf, works towards root.
verify2(Ownership, Root, [], Bounds) -> 
    true = in_bounds(Ownership, Bounds),
    Root;
verify2(Ownership, Root, [H|T], Bounds) ->
    #tree{
         rule = Rule,
         h1 = H1,
         h0 = H0
        } = H,
    Result = contract_direction(H, Ownership),%run_contract(H, Ownership, Dict),
    {Root, Bounds2}
        = if
              Result -> {H1, 
                        bounds_update(
                          Rule,
                          Bounds)};
              true -> {H0, 
                      bounds_update2(
                       Rule,
                       Bounds)}
          end,
    NewRoot = hash:doit(serialize_tree(H)),
    verify2(Ownership, NewRoot, T, Bounds2).


contract_direction(Tree, Owner) ->
    #owner{
            sortition_id = SID,
            pstart = <<PStart:256>>,%this moves too.
            pend = <<PEnd:256>>,
            priority = P,
            contracts = C
            %contract = CH1
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
        {priority, P2} ->
            P == P2;
        {priority_before, P2} ->
            P =< P2;
        {contract, C1} -> 
            is_in(C1, C)
    end.
is_in(X, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> 
    is_in(X, T).
all_in([], _) -> true;
all_in([H|T], L) -> 
    is_in(H, L) and
        all_in(T, L).


in_bounds(Ownership, Bounds) ->
    #owner{
           pstart = <<Ostart:256>>,
           pend = <<Oend:256>>,
           contracts = OC
          } = Ownership,
    #bounds{
             pstart = <<Bstart:256>>,
             pend = <<Bend:256>>,
             contracts = BC
           } = Bounds,
    if
        (Bstart > Ostart) ->
            io:fwrite("starts too early\n"),
            io:fwrite(packer:pack([<<Bstart:256>>, <<Ostart:256>>])),
            io:fwrite("\n"),
            io:fwrite(packer:pack([Bstart, Ostart])),
            io:fwrite("\n"),
            false;
        (Oend > Bend) ->
            io:fwrite("ends too late\n"),
            false;
        true ->
            all_in(BC, OC)
    end.
bounds_update({before, <<S:256>>}, 
              Bounds) ->
    S1 = Bounds#bounds.pend,
    S2 = min(S, S1),
    Bounds#bounds{
      pend = <<S2:256>>
     };
bounds_update({contract, CH}, 
              Bounds) ->
    CL1 = Bounds#bounds.contracts,
    B = is_in(CH, CL1),
    case B of
        true -> Bounds;
        false ->
            CL2 = [CH|CL1],
            Bounds#bounds{
              contracts = [CH|CL1]
             }
    end;
bounds_update({sid_before, _}, Bounds) -> Bounds;
bounds_update({sid, _}, Bounds) -> Bounds;
bounds_update({priority, _}, Bounds) -> Bounds;
bounds_update({priority_before, _}, Bounds) -> Bounds.

bounds_update2({before, <<S:256>>}, 
               Bounds) ->
    <<S1:256>> = Bounds#bounds.pstart,
    S2 = max(S, S1),
    Bounds#bounds{
      pstart = <<S2:256>>
     };
bounds_update2({contract, CH},
               Bounds) ->
    CH2 = contract_flip(CH),
    bounds_update(
      {contract, CH2},
      Bounds);
bounds_update2({sid_before, _}, Bounds) -> Bounds;
bounds_update2({sid, _}, Bounds) -> Bounds;
bounds_update2({priority, _}, Bounds) -> Bounds;
bounds_update2({priority_before, _}, Bounds) -> Bounds.

contract_flip(<<N:1, R:255>>) ->    
    N2 = case N of
             0 -> 1;
             1 -> 0
         end,
    <<N2:1, R:255>>.
            

serialize_tree(T) ->
    #tree{
           rule = {Type, C0},
           h0 = H0,
           h1 = H1
         } = T,
    {C, A} = case Type of
                 sid -> {C0, 1};
                 before -> {C0, 2};
                 priority -> {<<C0:8>>, 3};
                 sid_before -> {C0, 4};
                 priority_before -> {<<C0:8>>, 5};
                 contract -> {C0, 6}
        end,
                   
    <<C/binary, H0/binary, H1/binary, A:8>>.

serialize(X) ->
    PS = constants:pubkey_size(),
    HS = constants:hash_size(),
    #owner{
            pubkey = P,
            pubkey2 = P2,
            pstart = S,
            pend = E,
            priority = Pr,
            sortition_id = SID,
            contracts = C
      } = X,
    PS = size(P),
    PS = size(P2),
    32 = size(S),
    32 = size(E),
    %HS = size(C),
    HS = size(SID),
    CB = serialize_contracts(C, <<>>),
    <<P/binary,
      P2/binary,
      S/binary,
      E/binary,
      SID/binary,
      CB/binary,
      Pr:8>>.
serialize_contracts([], X) -> X;
serialize_contracts([H|T], X) ->
    <<_:256>> = H,
    X2 = <<X/binary, H/binary>>,
    serialize_contracts(T, X2).
   


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
             <<0:520>>,
             <<0:256>>,
             <<M3:256>>,
             0,
             SID,
             %[]),
            [hash:doit(1)]),
    X2 = new(keys:pubkey(),
             <<0:520>>,
             <<M3:256>>,
             <<M4:256>>,
             0,
             SID,
            []),
    X3 = new(keys:pubkey(),
             <<0:520>>,
             <<M4:256>>,
             <<M5:256>>,
             0,
             SID,
            []),
    X4 = new(keys:pubkey(),
             <<0:520>>,
             <<M5:256>>,
             <<M6:256>>,
             0,
             SID,
            []),
    X5 = new(keys:pubkey(),
             <<0:520>>,
             <<0:256>>,
             <<M6:256>>,
             0,
             SID2,
            []),
    SID3 = hash:doit(3),
    X6 = new(keys:pubkey(),
             <<0:520>>,
             <<0:256>>,
             <<M6:256>>,
             1,
             SID,
            []),
    %L1 = [X1, X2, X5],
    L2 = [X1, X2, X3, X4, X5, X6],
    %L2 = [X1, X2, X6],
    {Root, T} = make_tree(L2),
    Proof = lists:reverse(make_proof(X6, T)),
    Root = verify(X6, Proof),
    %{X1, Proof}.
    success.
    

-module(ownership2).
-export([%new/5,
         %make_root/1,

         pubkey/1,
         pstart/1,
         pend/1,
         contract/1,
         sid/1,
         
         verify/3,
         %is_between/2,

         serialize/1,
         deserialize/1
        ]).

%TODO
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

-record(x, {pubkey, %pubkey of who owns this probabilistic value space.
            pstart, %start of the probability space
            pend, %end of the probability space
            sortition_id,
            contract,
            evidence}).
new(P, S, E, C, Evidence, SID) ->
    #x{pubkey = P,
       pstart = S,
       pend = E,
       sortition_id = SID,
       contract = hash:doit(C),
       evidence = Evidence}.
pubkey(X) -> X#x.pubkey.
pstart(X) -> X#x.pstart.
pend(X) -> X#x.pend.
contract(X) -> X#x.contract.
sid(X) -> X#x.sortition_id.

-record(tree, {contract, 
               evidence, %facts we need to look up and include with the contract
               b1, h1, b0, h0}).
make_tree(Owners) ->
    %make a tree out of the owners.
    
    %{RootHash, MT}.
    ok.

make_proof(Owner, Tree) ->
    %grab all the elements leading towards Owner.
    ok.

verify(Ownership, Proof, Dict) ->
    %returns a root hash, so we can check that the Proof is linked to something else.
    %first off, check that the main contract returns "true". TODO
    X = hash:doit(serialize(Ownership)),
    verify2(Ownership, X, Proof, Dict).%starts from leaf, works towards root.
verify2(_, Root, [], _) -> Root;
verify2(Ownership, Root, [H|T], Dict) ->
    #tree{
           h1 = H1,
           h0 = H0
         } = H,
    Result = run_contract(H, Ownership, Dict),
    Root = case Result of
                 <<1:32>> -> H1;
                 <<0:32>> -> H0
             end,
    NewRoot = hash:doit(serialize_tree(H)),
    verify2(Ownership, NewRoot, T, Dict).

get_merkel_facts(Evidence, Dict) ->
    ok.

run_contract(Tree, Ownership, Dict) ->
    #x{
        sortition_id = SID,
        pstart = PStart,
        pend = PEnd
        } = Ownership,
    #tree{
           contract = Contract,
           evidence = Evidence
         } = Tree,
    Facts = get_merkel_facts(Evidence, Dict),
    SIDEvidence = [<<2:8>>, <<32:32>>, <<SID:256>>,%binary 32 SID
                   <<2:8>>, <<32:32>>, <<PStart:256>>,
                   <<2:8>>, <<32:32>>, <<PEnd:256>>], %binary 32 SID
    %TODO, we need to add the 
    OpGas = 10000,
    RamGas = 10000,
    Vars = 1000,
    Funs = 1000,
    Contract2 = SIDEvidence ++ Contract,
    State = chalang:new_state(99999999, 0, 0),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, Evidence, Contract, State, constants:hash_size(), 2, false),
    Data2 = chalang:run5(Facts, Data),
    Data3 = chalang:run5(Contract2, Data2),
    hd(chalang:stack(Data3)).


    
serialize_tree(T) ->
    #tree{
           contract = C,
           h0 = H0,
           h1 = H1
         } = T,
    CH = hash:doit(contract),
    <<CH/binary, H0/binary, H1/binary>>.

serialize(X) ->
    PS = constants:pubkey_size(),
    HS = constants:hash_size(),
    #x{
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
    #x{
        pubkey = <<P:PS>>,
        pstart = <<S:X>>,
        pend = <<E:X>>,
        sortition_id = <<SID:HS>>,
        contract = <<C:HS>>
      }.

-module(sortition_block_tx).
-export([go/4, make_dict/7]).
-include("../../records.hrl").
%-record(sortition_block_tx, {from, nonce, fee, id, validators, signatures, sid, height, state_root}).

make_dict(From, Fee, Validators, Sigs, Height, SR, SideHeight) ->
    Acc = trees:get(accounts, From),
    VR = sortition_new_tx:make_root(Validators),
    ID = hash:doit([SideHeight, VR]),
    #sortition_block_tx{
                     from = From, 
                     nonce = Acc#acc.nonce + 1,
                     fee = Fee,
                     id = ID,
                     validators = Validators,
                     signatures = Sigs,
                     height = Height,
                     side_height = SideHeight,
                     state_root = SR}.
go(Tx, Dict, NewHeight, _) ->
    #sortition_block_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    id = ID,
    validators = Validators,
    signatures = Sigs,
    height = Height,
    side_height = SideHeight,
    state_root = SR
   } = Tx,
    true = is_integer(SideHeight),
    VR = sortition_new_tx:make_root(Validators),
    ID = hash:doit([SideHeight, VR]),
    empty = sortition_block:dict_get(ID, Dict),
    if
        SideHeight == 0 -> ok;
        SideHeight > 0 -> 
            PrevID = hash:doit([SideHeight - 1, VR]),
            SB0 = sortition_block:dict_get(PrevID, Dict),
            OldSideHeight = SideHeight - 1,
            #sortition_block{
                              validators = VR,
                              side_height = OldSideHeight
             } = SB0
    end,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(A2, Dict),
    %S = sortition:dict_get(SID, Dict),
    %#sortition{
    %            validators = VR%verify that the validators list is correct for this sortition chain.
    %          } = S,
    SHH = hash:doit([SideHeight, SR]),
    true = verify_all(Validators, Sigs, SHH),
    SB = sortition_block:new(ID, VR, NewHeight, SideHeight, SR),
    sortition_block:dict_write(SB, Dict2).
    
    
verify_all([],[],_) -> true;
verify_all([V|VT], [S|ST], R) -> 
    A = sign:verify_sig(R, S, V),
    A and verify_all(VT, ST, R).

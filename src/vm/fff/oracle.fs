: map2 dupnil== if drop r> drop else 
       car r@ call >r swap r> swap cons swap recurse call then ;
: map >r nil swap map2 call reverse ; ( List Function -- List )
:reduce2 dupnil== if drop r> drop else 
  car >r swap r> r@ call swap recurse call then;
:reduce >r reduce2 call;  ( Y List Function -- X )

: sum2 + ;
: sum fraction 0 1 swap sum2 reduce call ; 
: normalize3 dupnil== if drop r> drop else
  car r@ / >r swap r> swap cons swap recurse call then ;
: normalize2 >r swap normalize3 call ;
: normalize dup sum call nil swap normalize2 call ;
(
: hd car ;
: tl cdr ;
)

: newWeights2 ( Wrong OldWeights PunishmentFraction -- NewWeights )
dupnil== if drop drop else
  swap car F @ ^ rot car rot * >r rot r> swap cons tuck recurse call then;
: newWeights F ! nil tuck newWeights2 call normalize call;

( [integer 2, integer 1, integer 1, integer 0] [fraction 1 1, fraction 1 1, fraction 1 1, fraction 1 1] normalize call fraction 1 2 newWeights call normalize call )

macro whelper == and if drop drop r> integer 1 + >r else;
:wrong2
dupnil== if drop drop r> else
( 2dup car swap car )
car rot car rot
2dup integer 1 == swap integer 0 whelper
2dup integer 0 == swap integer 1 whelper
2dup integer -1 == swap integer 0 whelper
2dup integer 0 == swap integer -1 whelper 
     == not if r> integer 2 + >r else 
then then then then then
(cdr swap cdr) recurse call then;

:wrong integer 0 >r wrong2 call;

([integer -1, integer 1, integer -1, integer 0]
[integer 1, integer 0, integer -1, integer 2]
wrong call )

:wrong_m2 dupnil== if drop drop r> else
car rot dup rot
wrong call r> cons >r swap recurse call then ;
:wrong_m nil >r wrong_m2 call reverse;

( [integer 1, integer 1]
[ [integer  1, integer 1],
  [integer  0, integer 1],
  [integer  0, integer 2],
  [integer -1, integer 1] ]
wrong_m call  )
macro pop dup @ car swap rot ! ;
: outcomes30 A @ B @ < if integer -1 else integer 1 then ;
: outcomes3 A @ B @ + C @ < if integer 0 else outcomes30 call then ;
: outcomes21 A @ B @ C @ + + D @ < if integer 2 else outcomes3 call then ;
: outcomes20 H @ nil == if outcomes21 call else 
  H pop
  dup integer 1 == if W pop A +! else then 
  dup integer -1 == if W pop B +! else then 
  dup integer 0 == if W pop C +! else then 
  dup integer 2 == if W pop D +! else then 
  drop recurse call then ;
: outcomes2 H ! W ! fraction 0 1 A ! fraction 0 1 B ! fraction 0 1 C ! fraction 0 1 D ! outcomes20 call;

( [integer 1,integer 2,integer 1,integer 0] [fraction 1 1, fraction 1 1, fraction 2 1, fraction 1 2] normalize call swap outcomes2 call )

: hd_map3 
  N @ integer 0 == if else
    r> swap cons swap r> swap cons swap
    integer -1 N +!
    recurse call
then;
macro hd_map2 nil hd_map3 call;
: hd_map dupnil== if hd_map2 else
  car car >r >r integer 1 N +! recurse call
then;

:outcomes dup car nil == if drop drop drop r> else 
drop hd_map call S !
(rot dup tuck swap)  outcomes2 call
r> cons >r (tl map call) swap dup tuck S @ recurse call
then ;
:outcomes0 nil >r outcomes call reverse;
:doit2 F ! 2dup
outcomes0 call Outcomes !
Outcomes @ swap wrong_m call swap F @ newWeights call
Outcomes @ ;
:doit ( NormalizedWeights VoteMatrix -- NewWeights Outcomes ) 
dup tuck
fraction 3 5 
doit2 call >r swap
fraction 5 7
doit2 call drop r>;


(
macro test
[integer 1,integer 2,integer 1,integer 0] [fraction 1 1, fraction 1 1, fraction 2 1, fraction 1 2] normalize call swap outcomes2 call 
;
)

(
macro test
[
  [integer  1,integer  1,integer  1,integer  1,integer 1,integer 2],
  [integer  1,integer  1,integer  1,integer  1,integer 1,integer 2],
  [integer -1,integer -1,integer  1,integer -1,integer 1,integer 2],
  [integer -1,integer -1,integer -1,integer  1,integer 1,integer 2],
  [integer -1,integer  1,integer -1,integer  1,integer 1,integer 2] ]
  hd_map call
;
)


macro test 
  [fraction 1 1,fraction 1 1,fraction 1 1,fraction 1 1,fraction 1 1] normalize call
[
  [integer  1,integer  1,integer  1,integer  1,integer 1,integer 2],
  [integer  1,integer  1,integer  1,integer  1,integer 1,integer 2],
  [integer -1,integer -1,integer  1,integer -1,integer 1,integer 2],
  [integer -1,integer -1,integer -1,integer  1,integer 1,integer 2],
  [integer -1,integer  1,integer -1,integer  1,integer 1,integer 2] ]
  doit call
;


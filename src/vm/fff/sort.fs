: map2 dupnil== if drop r> drop else 
  car r@ call >r swap r> swap cons swap recurse call then ;
: map >r nil swap map2 call reverse ; ( List Function -- List )

:merge2 
  dupnil== rot dupnil== rot and if drop drop r> else swap
  dupnil== if r> ++ else 
  swap dupnil== if swap r> reverse ++ else swap
  car >r swap 
  car r> 
  2dup > if else swap then r> cons >r rot cons recurse call
then then then;

:merge nil >r merge2 call swap drop;

:merge_setup2 nil cons ;
:merge_setup merge_setup2 map call ;
:merge_done car drop nil == ;
:doit2 dup merge_done call if car swap drop else 
  car swap car rot merge call nil cons swap ++ recurse call 
  (  car swap car rot nil cons swap ++ recurse call )
  then; 
:doit merge_setup call doit2 call;
 

( [integer 4, integer 13] [integer 2, integer 5, integer 10] swap merge call print )
( [integer 1, integer 3, integer 5, integer 9] [integer 2, integer 4, integer 8, integer 10] merge call  print )
( [integer 3, integer 2, integer 1] [integer 5, integer 2, integer 1] merge call )


macro test 
[integer 10, integer 2, integer 13, integer 4, integer 5] doit call 
[integer 2, integer 4, integer 5, integer 10, integer 13] ==
;

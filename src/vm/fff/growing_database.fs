macro db_root crash ;
: db_1 dup integer 5  == if drop integer 1 
    else db_root then ;
: db_2 dup integer 10 == if drop integer 2 
    else db_1 call then ;

macro test 
integer 5 db_2 call integer 1 ==
;
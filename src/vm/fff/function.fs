: square dup * ; 
: quad square call square call ; 


macro test
integer 2 quad call integer 16 ==
;
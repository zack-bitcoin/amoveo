macro end integer 1000 ;
macro start integer 100 ;
macro flip3 dup integer 100 / swap ( N -- M )
    dup integer 10 rem rot - integer 99 * + ;
macro palindrone dup end / swap ( N -- true/false )
    end rem flip3 == ;
: main 2dup start == swap ( N M --  )
            start == swap and
if
  drop drop Foo @ 
else
  dup start ==
  if
    drop integer 1 - end 
  else
    2dup * dup Foo @ <
    if
      drop
    else
      dup palindrone
      if Foo ! else drop
  then then then
  integer 1 - recurse call
then ;
: doit end end main call ;
doit call

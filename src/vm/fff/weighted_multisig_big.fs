( This is a weighted multisig with 5 keys.
)

macro Priv binary MVptai9RaW0zVHRGWjRrSitRdjVSeVF1b2w1aW1nbi9zQXdESHp2R1BYbz0= ;

macro Pub binary QkJRQkl0dUoxaCtxakRwa21ZbDhxR2dlcVVBLzJnK2NrQVNKQXZYT1BlbmVjWGJScDRkY0czR3dtSTBrb0kySjFoQlNyRHlIaklXOXE1MHFCWGdjMXI4PQ== ;

macro X binary YWJj ;
macroSign Sig Priv X
macro b X Pub verify_sig >r ;  
macro c r> if else drop integer 0 then ;
macro doit
  b b b b b
  integer 3 c
  integer 3 c
  integer 2 c
  integer 2 c
  integer 2 c
  + + + + integer 9 >
;

macro test
  Sig Sig Sig Sig Sig doit
;
(This is for commit reveal. The nonce for they are required to include, which is custom for this round. it is a very big random number, to avoid collisions, is 1337.
The number they committed to in secret is 55.
)
macro Priv binary MVptai9RaW0zVHRGWjRrSitRdjVSeVF1b2w1aW1nbi9zQXdESHp2R1BYbz0= ;

macro Pub binary QkJRQkl0dUoxaCtxakRwa21ZbDhxR2dlcVVBLzJnK2NrQVNKQXZYT1BlbmVjWGJScDRkY0czR3dtSTBrb0kySjFoQlNyRHlIaklXOXE1MHFCWGdjMXI4PQ== ;

: Func integer 55 integer 1337 ;

macroSign Sig Priv Func

: crf integer -10 integer -10 ;
macro commit_reveal swap dup crf match or_die call rot == or_die ;

macro test
Sig Func dup tuck Pub verify_sig or_die integer 1337 commit_reveal integer 55 == 
;
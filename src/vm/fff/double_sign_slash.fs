( this is a contract to punish people for signing contrary results. This contract would be used to stop oracles from outputing 2 contrary results. If it fails, it crashes.
   % 
)
macro Priv binary MVptai9RaW0zVHRGWjRrSitRdjVSeVF1b2w1aW1nbi9zQXdESHp2R1BYbz0= ;

macro Pub binary QkJRQkl0dUoxaCtxakRwa21ZbDhxR2dlcVVBLzJnK2NrQVNKQXZYT1BlbmVjWGJScDRkY0czR3dtSTBrb0kySjFoQlNyRHlIaklXOXE1MHFCWGdjMXI4PQ== ;

: func1 integer 55 integer 1337 ;
: func2 integer 54 integer 1337 ;

macroSign sign1 Priv func1
macroSign sign2 Priv func2

:crf integer -10 integer -10; 
macro commit_reveal swap dup crf match or_die call rot == or_die ;
macro double_signed_slash ( Sig1, Sig2, func1, func2, Pub1, constant -- )
          N !
          >r 
          2dup N @ commit_reveal >r
               N @ commit_reveal r> 
          == not or_die
	  swap tuck r@ 
	  verify_sig or_die r>
          verify_sig or_die;

macro test 
  sign1 sign2 func1 func2 Pub integer 1337 double_signed_slash true
;

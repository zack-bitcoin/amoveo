( Example of 2 of 3 multisig 
  The 2 channel owners are the first 2 participants, the pubkey embedded in the script is the third. )

macro Priv binary MVptai9RaW0zVHRGWjRrSitRdjVSeVF1b2w1aW1nbi9zQXdESHp2R1BYbz0= ;

macro Pub binary QkJRQkl0dUoxaCtxakRwa21ZbDhxR2dlcVVBLzJnK2NrQVNKQXZYT1BlbmVjWGJScDRkY0czR3dtSTBrb0kySjFoQlNyRHlIaklXOXE1MHFCWGdjMXI4PQ== ;

macro X binary YWJj ;

macroSign sig Priv X 

macro test
sig X Pub verify_sig true == 
;
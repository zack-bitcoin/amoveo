( this is a weighted multisig. The first 2 signatures are worth 3, and the last is worth 2. you need 6 total to pass. )

macro Priv binary MVptai9RaW0zVHRGWjRrSitRdjVSeVF1b2w1aW1nbi9zQXdESHp2R1BYbz0= ;

macro Pub binary QkJRQkl0dUoxaCtxakRwa21ZbDhxR2dlcVVBLzJnK2NrQVNKQXZYT1BlbmVjWGJScDRkY0czR3dtSTBrb0kySjFoQlNyRHlIaklXOXE1MHFCWGdjMXI4PQ== ;

macro X binary YWJj ;

macroSign sig Priv X 

macro b X Pub verify_sig rot ;

macro test
sig sig sig b b b 
if integer 3 else integer 0 then rot 
if integer 3 else integer 0 then rot 
if integer 2 else integer 0 then 
+ + integer 6 >
;
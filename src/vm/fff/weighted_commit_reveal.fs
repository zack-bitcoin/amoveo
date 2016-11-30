(This is a weighted multisig that uses the commit-reveal data structure, so that the participants can reveal simultaniously.
only include signatures from participants who are in the same direction. Use a 0 to replace people who didn't participate.
     input def2 0 Sig2 Func2 2 Sig1 Sig2 Func1 Func2 1
    option 0 is for a validator who did not sign, adds to bottom.
    option 1 is if a validator double-signed. This gets rid of their power for this round of validation.
    option 2 is for a validator who signed correctly.
    *the option number comes _after_ the signature and function.
)

macro Priv binary MVptai9RaW0zVHRGWjRrSitRdjVSeVF1b2w1aW1nbi9zQXdESHp2R1BYbz0= ;
macro Pub binary QkJRQkl0dUoxaCtxakRwa21ZbDhxR2dlcVVBLzJnK2NrQVNKQXZYT1BlbmVjWGJScDRkY0czR3dtSTBrb0kySjFoQlNyRHlIaklXOXE1MHFCWGdjMXI4PQ== ;
: func1 integer 1 integer 1337 ;
: func2 integer 2 integer 1337 ;
macroSign Sig1 Priv func1
macroSign Sig2 Priv func2
:crf integer -10 integer -10;
macro commit_reveal swap dup crf match or_die call rot == or_die ;
macro double_signed_slash ( Sig1, Sig2, func1, func2, Pub1, constant -- ) (crashes if it fails)
          N !
          >r 
          2dup N @ commit_reveal >r
               N @ commit_reveal r> 
          == not or_die
	  swap tuck r@ 
	  verify_sig or_die r>
          verify_sig or_die;

macro validator_signed
drop
     dup tuck pub @ verify_sig or_die 
     integer 1337 commit_reveal
     integer 1 == or_die ( only counting votes in same direction )
     B @ T ! 
;
macro validator_double_signed
  integer 1 == or_die
  pub @ integer 1337 double_signed_slash
  integer 0 T !
  integer 0 B !
  integer 1 Nonce +! 
;
macro validator_absent T ! ;

: main B ! pub ! 
dup integer 0 == if 
     validator_absent
else
   dup integer 2 == if 
     validator_signed
   else 
     validator_double_signed
   then
then B @ Bottom +! T @ Top +! ;

macro doit ( _read at top of page_ -- fraction_consensus Nonce ) ( the nonce should get added to the bet nonce, the highest nonce possible is the correct one. )
  integer 0 Nonce !
  Pub integer 2 main call
  Pub integer 2 main call
  Pub integer 3 main call
  Top @ Bottom @ i2f Nonce @
;

macro test
    integer 0 
    Sig1 func1 integer 2 
    Sig1 Sig2 func1 func2 integer 1 
    doit 
    integer 1 == swap
    fraction 2 5 == and
;
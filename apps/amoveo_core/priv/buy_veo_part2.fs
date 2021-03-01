( this allows us to use lists. )
macro [ nil ; 
macro , swap cons ; 
macro ] swap cons reverse ;

( this is the maximum value representable in chalang. )
( the payout vector that is used to divide up the money )
( from this contract, it's elements need to sum to )
( maximum )
macro maximum int 4294967295 ; 

( check that a conditional resulted in "true", )
( otherwise the contract should crash. )
macro or_die
  if
  else
    fail
  then ;

macro oracle_builder
  ( blockchain address amount ticker date -- oracle_text )
  >r >r >r >r >r
  ." The " r> ++
  ."  address " r> ++ ++
  ."  has received more than or equal to " r> ++ ++
  ."  of " r> ++ ++
  ."  before " r> ++ ++
;

macro oracle_id ( question_hash start_height -- oid)
int 0 dup ++ ++ swap ++ hash ;

( This is the static part of the second smart contract. )
: part2
    
( variables to customize this contract )
  Address !
  Date !
  Ticker !
  Amount !
  Blockchain !
  OracleStartHeight !

( grab the OID from the consensus state. )
  car drop
  car swap drop
  car swap drop
  car drop
  int 32 split
  OID !
  ( get the one-byte result of the oracle, )
  ( convert to a 4 byte integer)
  int 1 split swap drop
  binary 3 AAAA swap ++ ( 3 bytes of zeros )
  OracleResult !
  
  ( Date @ Ticker @ Amount @ Address @ Blockchain @ )
  Blockchain @ Address @ Amount @ Ticker @ Date @ 
  oracle_builder hash ( now we have the question hash )
  
  ( generate OID from oracle question )
  OracleStartHeight @ oracle_id OID2 !
  
  ( checking that the oids match )
  OID @ OID2 @ =2 or_die
  
  OracleResult @ int 1 =2
  if ( result of oracle is "true" so the bitcoin arrived in time )
    ( give the money to type 2 )
    [ int 0 , maximum ]
    int 0 int 1000
  else
    OracleResult @ int 2 =2
    if ( result of oracle is "false" so the bitcoin did not arrive in time )
      ( give the money to type 1 )
      [ maximum , int 0 ]
      int 0 int 1000
    else
      maximum int 2 / half !
      [ half @ , maximum half @ - ]
      OracleResult @ int 3 =2
      if ( bad question )
        ( split the money 50-50 )
        int 0 int 1000
      else ( oracle unresolved )
        ( keep waiting for the oracle to resolve )
        maximum int 1
      then
    then
  then
;

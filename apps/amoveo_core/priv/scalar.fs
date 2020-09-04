( this allows us to use lists. )
macro [ nil ; 
macro , swap cons ; 
macro ] swap cons reverse ;

( this is the maximum value representable in chalang. the payout vector that is used to divide up the money from this contract, it's elements need to sum to maximum )
macro maximum int 4294967295 ; 

( defines the empty string, which we need to end our recursion )
macro empty_string int 4 int 0 split swap drop ; 

( check that a conditional resulted in "true", otherwise the contract should crash. )
macro or_die
if
else
    fail
then ;

macro convert_digit ( converts a one-digit integer into a one byte string representation of that integer )
int 48 + int 3 split drop ; 

: int_to_string2 ( string int -- string) 
    int 10 ddup / tuck rem 
    convert_digit 
    rot ++ swap 
    int 0 ==
    if
        drop drop
    else
        drop recurse call
    then ; 
      
macro int_to_string ( int -- string )
( converts a multi-digit integer into a string representation )
int 0 == if drop drop 
    binary 1 MA== 
else drop 
    empty_string swap int_to_string2 call 
then ;

( loads the settings used to customize this contract )
TextPart !
OracleStartHeight ! 

( loads the evidence provided by whoever is trying to close this contract )
swap Price ! 

( calculates the hash of the question that we ask the oracle in order to resolve this contract )
macro QuestionHash 
 TextPart @ Price @ int_to_string ++ hash ;

( calculates the oracle id for the oracle that will determine the result of this contract )
macro OracleID
OracleStartHeight @ int 0 dup ++ ++ QuestionHash ++ hash ;

( check that this oracle data is for the right oracle )
car drop
car swap drop
car swap drop
car drop
int 32 split 
OracleID =2 or_die

( get the one-byte result of the oracle)
int 1 split swap drop
( check that it is equal to <<1>>, which is the result for "true". base64:encode <<1>> is AQ== )
binary 1 AQ== =2 or_die

( divide up the money according to the price written in the oracle. )
[ Price @ , maximum Price @ - ] 
int 0 int 1000


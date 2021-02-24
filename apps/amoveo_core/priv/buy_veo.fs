( this allows us to use lists. )
macro [ nil ; 
macro , swap cons ; 
macro ] swap cons reverse ;

( this is the maximum value representable in chalang. the payout vector that is used to divide up the money from this contract, it's elements need to sum to maximum )
macro maximum int 4294967295 ; 

( Se need an empty string to end our recursion )
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
int 0 ==
if drop drop 
    binary 1 MA== 
else drop 
    empty_string swap int_to_string2 call 
then ;

( for measuring the number of bytes in a binary )
: bin_length2 ( accumulator bin -- length )
    dup empty_string =2
    if
        drop
    else
        int 1 split drop
        swap int 1 + swap
        recurse call
    then ;
macro bin_length ( bin -- length )
  int 0 swap bin_length2 call ;

macro oracle_builder ( date ticker amount address blockchain -- oracle_text )
    ." The " swap ++
    ." address " swap ++
    ." has received more than or equal to " swap ++
    ." of " swap ++
    ." before " swap ++
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
( get the one-byte result of the oracle, convert to a 4 byte integer)
    int 1 split swap drop
    AAAA ++ ( 3 bytes of zeros )
    OracleResult !

    Date @ Ticker @ Amount @ Address @ Blockchain @ 
    oracle_builder hash ( now we have the question hash )

    OracleStartHeight @ oracle_id OID2 ! ( generated OID from oracle question )

    OID @ OID2 @ =2 or_die ( checking that the oids match )

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


( variables to customize this contract )
ReceiptID !
Date !
Ticker !
Amount !
Blockchain !
OracleStartHeight !
ProvideAddressTimeout !

( if they don't provide a bitcoin address in time, then give the veo to type 1. )
ProvideAddressTimeout @ height <
if
    [ maximum , int 0 ]
    int 0 int 1000
    return
else
then

( evidence to end this contract )
Address !
AddressSig !

( loading the trade receipt from consensus state, because only the person who accepted this swap request can choose the address to receive their cryptocurrency on the other blockchain. )
car drop
car swap drop
car swap drop
car drop
int 32 split ReceiptID =2 or_die
int 65 Acc2 !
drop

( check that Acc2 signed over Address where they want to receive their BTC or whatever )
AddressSig @ Address @ Acc2 @ verify_sig or_die

( type 1 of first contract pays out to type 1 of second contract. type 2 of first contract pays out to type 2 of second contract )
[ [ max , int 0 ] ,
[ int 0 , max ] ]

( generating the root hash of the second smart contract )
( OracleStartHeight Blockchain Amount Ticker Date Address part2 call )
macro int_op AA== ;
macro bin_op Ag== ;
macro call_op cQ== ;
int_op OracleStartHeight @ ++
bin_op ++ Blockchain @ bin_length ++ Blockchain @ ++
bin_op ++ Amount @ bin_length ++ Amount @ ++
bin_op ++ Ticker @ bin_length ++ Ticker @ ++
bin_op ++ Date @ bin_length ++ Date @ ++
bin_op ++ Address @ bin_length ++ Address @ ++
bin_op ++ int 32 ++ part2 ++ call_op ++
hash

int 0 int 1000

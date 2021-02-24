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


( specify in the oracle that the limit order to accept the contract and the contract evidence to load the bitcoin address, they must both happen in the same multi-tx )


macro oracle_builder ( date ticker amount address blockchain -- oracle_text )
    ." The " swap ++
    ." address " swap ++
    ." has received more than or equal to " swap ++
    ." of " swap ++
    ." before " swap ++
;

macro oracle_id ( question_hash start_height -- oid)
  int 0 dup ++ ++ swap ++ hash ;

( when the contract is created the creator needs to decide: )
( 1 how much veo the creator is sending )
( 2 what currency the creator wants to receive )
( 3 what blockchain the creator wants to receive it on )
( 4 how much time the acceptor has to accept this offer )
( 5 how much time the creator has to send the bitcoin after the offer is accepted )

( 1 4 is decided by the limit order )
( 2 3 5 is in the oracle )

: part2 

( variables to customize this contract )
    Address !
    Date !
    Ticker !
    Amount !
    Blockchain !
    OracleStartHeight !

( swap Result ! )

    car drop
    car swap drop
    car swap drop
    car drop
    int 32 split
( grabbed the OID from the consensus state. )

    Date @ Ticker @ Amount @ Address @ Blockchain @ 
    oracle_builder hash ( now we have the question hash )

    OracleStartHeight @ oracle_id ( generated OID from oracle question )

    =2 or_die ( checking that the oids match )

( get the one-byte result of the oracle)
    int 1 split swap drop
( check that it is equal to <<1>>, which is the result for "true". base64:encode <<1>> is AQ== )
    dup binary 1 AQ== =2
    if ( result of oracle is "true" )
    ( give the money to type 2 )
        [ int 0 , maximum ]
        int 0 int 1000
    else
        binary 1 Ag== =2
        if ( result of oracle is "false" )
    ( give the money to type 1 )
            [ maximum , int 0 ]
            int 0 int 1000
        else ( oracle unresolved, or "bad question" )
            fail 
        then
    then
;


( verify that the caller is the same person who accepted the swap offer )

( variables to customize this contract )
TradeID !
Date !
Ticker !
Amount !
Blockchain !
OracleStartHeight !
ProvideAddressTimeout !

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


car drop
car swap drop
car swap drop
car drop
int 32 split TradeID =2 or_die
int 65 Acc2 !
drop

( check that Acc2 signed over Address )
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
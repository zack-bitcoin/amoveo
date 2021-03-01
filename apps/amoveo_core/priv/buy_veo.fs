( this allows us to use lists. )
macro [ nil ; 
macro , swap cons ; 
macro ] swap cons reverse ;

( this is the maximum value representable in chalang. 
 the payout vector that is used to divide up the money 
 from this contract, it's elements need to sum to 
 maximum )
macro maximum int 4294967295 ; 

( check that a conditional resulted in "true", )
( otherwise the contract should crash. )
macro or_die
  if
  else
    fail
  then ;

( We need an empty string to end our recursion )
macro empty_string int 4 int 0 split swap drop ;

( variables to customize this contract )
TradeID !
TradeNonce !
Date !
Ticker !
Amount !
Blockchain !
OracleStartHeight !
ProvideAddressTimeout !


( if they don't provide a bitcoin address in time, )
( then give the veo to type 2. )
ProvideAddressTimeout @ height <
if
  [ int 0, maximum ]
  int 0 int 1000
  return
else
then

( evidence to end this contract )
swap Address !
swap AddressSig !

( loading the trade receipt from consensus state, )
( because only the person who accepted this swap )
( request can choose the address to receive their )
( cryptocurrency on the other blockchain. )
car drop
car swap drop
car swap drop
car drop
int 32 split TradeID @ =2 or_die
int 65 split Acc2 !
TradeNonce @ =2 or_die

( check that Acc2 signed over Address where they want )
( to receive their BTC or whatever )
AddressSig @ Address @ Acc2 @ verify_sig or_die


( type 1 of first contract pays out to type 1 of second )
( contract. type 2 of first contract pays out to type 2 )
( of second contract )
[ [ maximum , int 0 ] ,
[ int 0 , maximum ] ]

( generating the root hash of the second smart contract )
( OracleStartHeight Blockchain Amount 
Ticker Date Address part2 call )
macro int_op binary 1 AA== ;
macro bin_op binary 1 Ag== ;
macro call_op binary 1 cQ== ;


( for measuring the number of bytes in a binary )
def ( bin accumulator -- length )
  swap
  dup empty_string =2
  if
    drop
  else
    int 1 split drop
    swap int 1 +
    recurse call
  then ;
bin_length_fun !
macro bin_length ( bin -- length )
  int 0 bin_length_fun @ call ;

( this anonymous function converts a binary value into
 chalang code for loading that same binary 
value into a different chalang VM instance. 
This is how we write a chalang contract in 
this contract. )
def
  @ >r bin_op r@ bin_length ++ r> ++ ++
;
bin_code_fun !

macro bin_code bin_code_fun @ call ;

int_op OracleStartHeight @ ++
Blockchain bin_code
Amount bin_code
Ticker bin_code
Date bin_code
Address bin_code
bin_op ++ int 32 ++ part2 ++ call_op ++
( print )
hash

( part2 print print drop)

int 0 int 1000

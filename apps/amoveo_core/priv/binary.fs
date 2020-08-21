( load the settings for this contract)
OID ! 

( this allows us to use list format )
macro [ nil ; 
macro , swap cons ; 
macro ] swap cons reverse ;

( the maximum representable value in chalang we need to make sure that the payout vector sums to this. )
macro maximum int 4294967295 ;

( unpack the oracle )
 car drop car swap drop car swap drop car drop 
int 32 split
( verify that this evidence is for the correct oracle id )
 OID @ 
== if else fail then

( extract the one-byte result of the oracle )
drop drop int 1 split swap drop
( convert the result to integer format )
binary 3 AAAA swap ++

( based on the result of the oracle, distribute the funds. PayoutVector Delay Nonce )
int 3 == if 
    [ int 0 , int 0 , maximum ]
    int 0 int 1000 
else drop 
    int 1 == if 
        [ maximum , int 0 , int 0 ]
        int 0 int 1000 
    else drop 
        int 2 == if 
            [ int 0 , maximum, int 0 ]
            int 0 int 1000 
        else drop drop 
            [ int 2147483648 ,
            int 2147483647 ,
            int 0 ]
            int 5000 int 10 
        then 
    then
then
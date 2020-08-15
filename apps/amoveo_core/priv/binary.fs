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
int1 32 split
( verify that this evidence is for the correct oracle id )
 OID @ 
== if else fail then

( extract the one-byte result of the oracle )
drop drop int1 1 split swap drop
( convert the result to integer format )
binary 3 AAAA swap ++

( based on the result of the oracle, distribute the funds. PayoutVector Delay Nonce )
int1 3 == if 
    [ int1 0 , int1 0 , maximum ]
    int1 0 int2 1000 
else drop 
    int 1 == if 
        [ maximum , int1 0 , int1 0 ]
        int1 0 int2 1000 
    else drop 
        int1 2 == if 
            [ int1 0 , maximum, int1 0 ]
            int1 0 int2 1000 
        else drop drop 
            [ int 2147483648 ,
            int 2147483647 ,
            int1 0 ]
            int2 5000 int1 10 
        then 
    then
then
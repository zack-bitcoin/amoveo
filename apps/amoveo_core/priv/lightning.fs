
drop
stack_size int4 0 ==
if
 int4 100 int4 1 int4 0 return ( delay nonce amount )
else then ( S 1 0 )
drop drop ( S )
hash r> ( hashS SecretHash )
== swap drop swap drop ( B )
if
 int4 0 int4 2 int4 10000 ( delay nonce amount )
else
 int4 100 int4 1 int4 0 ( delay nonce amount )
then
return

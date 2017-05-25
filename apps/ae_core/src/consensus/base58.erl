-module(base58).
-export([check_base58/1, binary_to_base58/1, integer_to_base58/1, base58_to_integer/1, base58_to_binary/1]).

%% @doc This is an internal function that encodes the equivalent base58 number
%% to the corresponding alphabet character.
%%
%% @spec b58char(integer()) -> 'error' | byte()

-spec b58char(integer()) -> 'error' | byte().
b58char(0) -> $1;
b58char(1) -> $2;
b58char(2) -> $3;
b58char(3) -> $4;
b58char(4) -> $5;
b58char(5) -> $6;
b58char(6) -> $7;
b58char(7) -> $8;
b58char(8) -> $9;
b58char(9) -> $A;
b58char(10) -> $B;
b58char(11) -> $C;
b58char(12) -> $D;
b58char(13) -> $E;
b58char(14) -> $F;
b58char(15) -> $G;
b58char(16) -> $H;
b58char(17) -> $J;
b58char(18) -> $K;
b58char(19) -> $L;
b58char(20) -> $M;
b58char(21) -> $N;
b58char(22) -> $P;
b58char(23) -> $Q;
b58char(24) -> $R;
b58char(25) -> $S;
b58char(26) -> $T;
b58char(27) -> $U;
b58char(28) -> $V;
b58char(29) -> $W;
b58char(30) -> $X;
b58char(31) -> $Y;
b58char(32) -> $Z;
b58char(33) -> $a;
b58char(34) -> $b;
b58char(35) -> $c;
b58char(36) -> $d;
b58char(37) -> $e;
b58char(38) -> $f;
b58char(39) -> $g;
b58char(40) -> $h;
b58char(41) -> $i;
b58char(42) -> $j;
b58char(43) -> $k;
b58char(44) -> $m;
b58char(45) -> $n;
b58char(46) -> $o;
b58char(47) -> $p;
b58char(48) -> $q;
b58char(49) -> $r;
b58char(50) -> $s;
b58char(51) -> $t;
b58char(52) -> $u;
b58char(53) -> $v;
b58char(54) -> $w;
b58char(55) -> $x;
b58char(56) -> $y;
b58char(57) -> $z;
b58char(_) -> error.

%% @doc This is an internal function that decodes a base58 character into its equivalent value
%%
%% @spec charb58(byte()) -> 'error' | integer()

-spec charb58(byte()) -> 'error' | integer().
charb58($1) -> 0;
charb58($2) -> 1;
charb58($3) -> 2;
charb58($4) -> 3;
charb58($5) -> 4;
charb58($6) -> 5;
charb58($7) -> 6;
charb58($8) -> 7;
charb58($9) -> 8;
charb58($A) -> 9;
charb58($B) -> 10;
charb58($C) -> 11;
charb58($D) -> 12;
charb58($E) -> 13;
charb58($F) -> 14;
charb58($G) -> 15;
charb58($H) -> 16;
charb58($J) -> 17;
charb58($K) -> 18;
charb58($L) -> 19;
charb58($M) -> 20;
charb58($N) -> 21;
charb58($P) -> 22;
charb58($Q) -> 23;
charb58($R) -> 24;
charb58($S) -> 25;
charb58($T) -> 26;
charb58($U) -> 27;
charb58($V) -> 28;
charb58($W) -> 29;
charb58($X) -> 30;
charb58($Y) -> 31;
charb58($Z) -> 32;
charb58($a) -> 33;
charb58($b) -> 34;
charb58($c) -> 35;
charb58($d) -> 36;
charb58($e) -> 37;
charb58($f) -> 38;
charb58($g) -> 39;
charb58($h) -> 40;
charb58($i) -> 41;
charb58($j) -> 42;
charb58($k) -> 43;
charb58($m) -> 44;
charb58($n) -> 45;
charb58($o) -> 46;
charb58($p) -> 47;
charb58($q) -> 48;
charb58($r) -> 49;
charb58($s) -> 50;
charb58($t) -> 51;
charb58($u) -> 52;
charb58($v) -> 53;
charb58($w) -> 54;
charb58($x) -> 55;
charb58($y) -> 56;
charb58($z) -> 57;
charb58(_) -> error.

%% @doc Check to see if a passed Base58 string contains the correct characters.
%% 'true' will be returned if the string contains the correct characters and
%% 'false' will be returned otherwise.
%%
%% @spec check_base58(base58()) -> boolean()
%% @type base58() = string().

-spec check_base58(base58()) -> boolean().
-type base58() :: string().
check_base58(Base58) ->
    lists:all(fun(C) -> C =/= 'error' end, lists:map(fun(C) -> charb58(C) end, Base58)).

%% @doc Convert an unsigned integer into its Base58 equivalent.
%%
%% @spec integer_to_base58(integer()) -> 'error' | base58()

-spec integer_to_base58(integer()) -> 'error' | base58().
integer_to_base58(0) -> [];
integer_to_base58(Integer) ->
    Quot = Integer div 58,
    Rem = Integer rem 58,
    integer_to_base58(Quot) ++ [b58char(Rem)].

%% @doc Convert a Base58 string into a unsigned integer value. This is an
%% internal function that is not exposed to the user.
%%
%% @spec base58_to_integer(char(),base58()) -> 'error' | integer()

-spec base58_to_integer(char(),base58()) -> 'error' | integer().
base58_to_integer(C, []) -> C;
base58_to_integer(C, [X | Xs]) ->
    base58_to_integer(C * 58 + charb58(X), Xs).

%% @doc Convert a Base58 string into a unsigned integer value.
%% The Base58 string must be encoded in a big-endian representation.
%%
%% @spec base58_to_integer(base58()) -> 'error' | integer()

-spec base58_to_integer(base58()) -> 'error' | integer().
base58_to_integer([]) -> error;
base58_to_integer([Char]) -> charb58(Char);
base58_to_integer([Char | Str]) ->
    base58_to_integer(charb58(Char), Str).

%% @doc Convert a Base58 string into the binary equivalent of the unsigned
%% integer representation. The Base58 string must be encoded in a big-endian
%% representation.
%%
%% @spec base58_to_binary(base58()) -> binary()

-spec base58_to_binary(base58()) -> binary().
base58_to_binary(Base58) ->
    Bin = binary:encode_unsigned(base58_to_integer(Base58)),
    %The conversion between the binary and the integer strips any leading zero bytes that 
    % might have appeared in the binary - '0's' should be prepended to the binary stream for each
    % 1 that appeared at the start of the base58 string.
    zeroPad(Base58, Bin).

%% @doc Convert a binary into a Base58 encoded string. The resulting Base58
%% encoded string will be in a big-endian representation of the original binary.
%%
%% @spec binary_to_base58(binary()) -> 'error' | base58()

-spec binary_to_base58(binary()) -> 'error' | base58().
binary_to_base58(Binary) when is_binary(Binary) ->
    case integer_to_base58(binary:decode_unsigned(Binary)) of
	error -> error;
	Base58 ->
	    % see above comment - just the reverse
	    binaryPad(binary_to_list(Binary), Base58)
		end.

%% @doc Pad a "1" character to a Base58 stream to account for any stripped zeros
%%
%% @spec binaryPad(list(), base58())
binaryPad([0 | Rest], Bin) ->
    binaryPad(Rest, "1" ++ Bin);
binaryPad(_, Bin) -> Bin.

%% @doc Pad a zero byte to a Base58 stream to account for any leading 1's
%%
%% @spec zeroPad(base58(), binary())

zeroPad("1" ++ Rest, Base58) ->
    zeroPad(Rest, <<0, Base58/binary>>);
zeroPad(_, Base58) -> Base58.



    
	

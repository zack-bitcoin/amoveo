-module(gas_price).
-export([test/0, timer0/0, timer1/0, timer2/0, timer3/0, timer4/0, timer5/0, timer6/0, timer7/0]).
timer0() -> %4 microseconds
    [{integer, 1}].
timer1() -> %17
    compiler:compile(<<" 
1 2 swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap swap 
">>).
timer2() -> %352
    compiler:compile(<<" 
:b YWJj
hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash hash 
">>).
timer3() -> %24
    compiler:compile(<<" 
:i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 :i 1 
">>).
timer4() -> %550
    Swaps = compiler:compile(<<" 
: a 1 ;
: b 1 ;
: ac 1 ;
: bc 1 ;
: da 1 ;
: db 1 ;
: dac 1 ;
: dbc 1 ;
: ea 1 ;
: eb 1 ;
: eac 1 ;
: ebc 1 ;
: eda 1 ;
: edb 1 ;
: edac 1 ;
: edbc 1 ;
: af 1 ;
: fb 1 ;
: fac 1 ;
: fbc 1 ;
: fda 1 ;
: fdb 1 ;
: fdac 1 ;
: fdbc 1 ;
: fea 1 ;
: feb 1 ;
: feac 1 ;
: febc 1 ;
: edfa 1 ;
: edfb 1 ;
: fedac 1 ;
: fedbc 1 ;
: ag 1 ;
: gb 1 ;
: gac 1 ;
: gbc 1 ;
: gda 1 ;
: gdb 1 ;
: gdac 1 ;
: gdbc 1 ;
: gea 1 ;
: geb 1 ;
: geac 1 ;
: gebc 1 ;
: geda 1 ;
: gedb 1 ;
: gedac 1 ;
: gedbc 1 ;
: gaf 1 ;
: gfb 1 ;
: gfac 1 ;
: gfbc 1 ;
: gfda 1 ;
: gfdb 1 ;
: gfdac 1 ;
: gfdbc 1 ;
: gfea 1 ;
: gfeb 1 ;
: gfeac 1 ;
: gfebc 1 ;
: gedfa 1 ;
: gedfb 1 ;
: gfedac 1 ;
: gfedbc 1 ;
">>),
    Swaps.
timer5() -> %257
    Swaps = compiler:compile(<<" 
: a 1 ;
: b 1 ;
: ac 1 ;
: bc 1 ;
: da 1 ;
: db 1 ;
: dac 1 ;
: dbc 1 ;
: ea 1 ;
: eb 1 ;
: eac 1 ;
: ebc 1 ;
: eda 1 ;
: edb 1 ;
: edac 1 ;
: edbc 1 ;
: af 1 ;
: fb 1 ;
: fac 1 ;
: fbc 1 ;
: fda 1 ;
: fdb 1 ;
: fdac 1 ;
: fdbc 1 ;
: fea 1 ;
: feb 1 ;
: feac 1 ;
: febc 1 ;
: edfa 1 ;
: edfb 1 ;
: fedac 1 ;
: fedbc 1 ;
">>),
    Swaps.
timer6() -> %334
    compiler:compile(<<"
 : func dup :i 0 == if else :i 1 - recurse call then ;
 :i 64 func call
">>).
timer7() -> %27
    X = <<":b QkpiMzFWb2U2a0hkWGxFbVRkSzhueGVPeHAvVUs3enJUdFRaMWFWUmxFL2d4TldqMlJlYngrQm1IZ0RuVGU4MHNxMWZSTVBSWGpFNW55N2N3OWU0ckF3PQ==
:b QkpiMzFWb2U2a0hkWGxFbVRkSzhueGVPeHAvVUs3enJUdFRaMWFWUmxFL2d4TldqMlJlYngrQm1IZ0RuVGU4MHNxMWZSTVBSWGpFNW55N2N3OWU0ckF3PQ==
:b QkpiMzFWb2U2a0hkWGxFbVRkSzhueGVPeHAvVUs3enJUdFRaMWFWUmxFL2d4TldqMlJlYngrQm1IZ0RuVGU4MHNxMWZSTVBSWGpFNW55N2N3OWU0ckF3PQ==
:b QkpiMzFWb2U2a0hkWGxFbVRkSzhueGVPeHAvVUs3enJUdFRaMWFWUmxFL2d4TldqMlJlYngrQm1IZ0RuVGU4MHNxMWZSTVBSWGpFNW55N2N3OWU0ckF3PQ==
">>,
    Y = << X/binary, X/binary, X/binary, X/binary >>,
    Z = << Y/binary, Y/binary, Y/binary, Y/binary >>,
    compiler:compile(Z).
    
test() ->
    [timer:tc(language, run, [timer0(), 10000]),%3 microseconds
     timer:tc(language, run, [timer1(), 10000]),%14
     timer:tc(language, run, [timer2(), 10000]),%338
     timer:tc(language, run, [timer3(), 10000]),%21
     timer:tc(language, run, [timer4(), 10000]),%686
     timer:tc(language, run, [timer5(), 10000]),%296
     timer:tc(language, run, [timer6(), 10000]),%364
     timer:tc(language, run, [timer7(), 10000])%28
].

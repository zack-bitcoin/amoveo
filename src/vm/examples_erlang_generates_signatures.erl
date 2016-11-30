-module(examples_erlang_generates_signatures).
-export([test/0]).

testF(EPub, Priv) ->
    %Example of 2 of 3 multisig 
    %The 2 channel owners are the first 2 participants, the pubkey embedded in the script is the third.
    Sig0 = base64:encode(testnet_sign:sign(<<"abc">>, Priv)),
    F = compiler:compile(<< <<" binary ">>/binary, Sig0/binary,
      <<"    binary YWJj
	     binary ">>/binary,  EPub/binary, <<"
             print
	     verify_sig
	     ">>/binary >>),
    [true] = language:run(F, 1000).
testG(EPub, Priv) ->
% this is a weighted multisig. The first 2 signatures are worth 3, and the last is worth 2. you need 6 total to pass.
    Sig = testnet_sign:sign(<<"abc">>, Priv),
    B = compiler:compile(<< <<"macro b binary YWJj binary ">>/binary, EPub/binary, <<" verify_sig rot ;  b b b
if integer 3 else integer 0 then rot 
if integer 3 else integer 0 then rot 
if integer 2 else integer 0 then 
+ + integer 6 >
       ">>/binary >>),
    [true] = language:run([Sig|[Sig|[Sig|B]]], 1000).
testH(EPub, Priv) -> 
%This is for commit reveal. The nonce for they are required to include, which is custom for this round. it is a very big random number, to avoid collisions, is 1337
%The number they committed to in secret is 55.
    Func = <<" integer 55 integer 1337">>,
    C = hash:doit(compiler:compile(Func)),
    Sig = base64:encode(testnet_sign:sign(C, Priv)),
    D = compiler:compile(<< <<" : func " >>/binary, Func/binary, <<" ; 
: crf integer -10 integer -10 ;
macro commit_reveal swap dup crf match or_die call rot == or_die ;
     binary ">>/binary, Sig/binary,  
 <<" func dup tuck binary ">>/binary, EPub/binary,
     % <<" verify_sig or_die dup integer 2 match integer -10 integer -10 or_die
     <<" verify_sig or_die integer 1337 commit_reveal ">>/binary >>),
    [55] = language:run(D, 2000).
testI(EPub, Priv) ->
       % here is a contract to punish people for signing contrary results. This contract would be used to stop oracles from outputing 2 contrary results.
   % Sig1, Sig2, func1, func2, Pub1, constant. <--input top ->
   Func1 = <<" integer 55 integer 1337 ">>,
   Func2 = <<" integer 54 integer 1337 ">>,
   DFunc1 = << <<" : func1 " >>/binary, Func1/binary, <<" ; ">>/binary >>,
   DFunc2 = << <<" : func2 " >>/binary, Func2/binary, <<" ; ">>/binary >>,
   C1 = hash:doit(compiler:compile(Func1)),
   C2 = hash:doit(compiler:compile(Func2)),
   Sign1 = base64:encode(testnet_sign:sign(C1, Priv)),
   Sign2 = base64:encode(testnet_sign:sign(C2, Priv)),
   E = compiler:compile(<< DFunc1/binary, DFunc2/binary, 
	       <<" :crf integer -10 integer -10; 
macro commit_reveal swap dup crf match or_die call rot == or_die ;
macro double_signed_slash           
          N !
          >r 
          2dup N @ commit_reveal >r
               N @ commit_reveal r> 
          == not or_die
	  swap tuck r@ 
	  verify_sig or_die r>
          verify_sig or_die;

binary ">>/binary, Sign1/binary, 
	       <<" binary ">>/binary, Sign2/binary, 
	       <<" binary ">>/binary, (base64:encode(C1))/binary,
	       <<" binary ">>/binary, (base64:encode(C2))/binary,
	       <<" binary ">>/binary, EPub/binary, 
               <<" integer 1337 
          double_signed_slash 
    ">>/binary >>),
    [] = language:run(E, 8000).
testJ(EPub, Priv) ->
%This is a weighted multisig with 5 keys.
    Sig = testnet_sign:sign(<<"abc">>, Priv),
    B = compiler:compile(<< <<"macro b binary YWJj binary ">>/binary, EPub/binary, <<" verify_sig >r ;  
macro c r> if else drop integer 0 then ;
b b b b b
integer 3 c
integer 3 c
integer 2 c
integer 2 c
integer 2 c
+ + + + integer 9 >
       ">>/binary >>),
    [true] = language:run([Sig|[Sig|[Sig|[Sig|[Sig|B]]]]], 3000).
testK(EPub, Priv) ->
%This is a weighted multisig that uses the commit-reveal data structure, so that the participants can reveal simultaniously.
%only include signatures from participants who are in the same direction. Use a 0 to replace people who didn't participate.
    % input def2 0 Sig2 Func2 2 Sig1 Sig2 Func1 Func2 1
    %option 0 is for a validator who did not sign, adds to bottom.
    Func = <<" integer 1 integer 1337 ">>,
    Func2 = <<" integer 2 integer 1337 ">>,
   DFunc = << <<" : func " >>/binary, Func/binary, <<" ; ">>/binary >>,
   DFunc2 = << <<" : func2 " >>/binary, Func2/binary, <<" ; ">>/binary >>,
    C = hash:doit(compiler:compile(Func)),
    C2 = hash:doit(compiler:compile(Func2)),
    Sig = base64:encode(testnet_sign:sign(C, Priv)),
    Sig2 = base64:encode(testnet_sign:sign(C2, Priv)),
    A = compiler:compile(<< DFunc/binary, DFunc2/binary, 
	       <<" integer 0 ">>/binary, 
		   <<" binary ">>/binary, Sig/binary, 
		   <<" binary ">>/binary, (base64:encode(C))/binary, 
               <<" integer 2 ">>/binary, 
		   <<" binary ">>/binary, Sig/binary, 
		   <<" binary ">>/binary, Sig2/binary, 
		   <<" binary ">>/binary, (base64:encode(C))/binary, 
		   <<" binary ">>/binary, (base64:encode(C2))/binary, 
               <<" integer 1 ">>/binary, 

<<"
   :crf integer -10 integer -10;
macro commit_reveal swap dup crf match or_die call rot == or_die ;
macro double_signed_slash           
          N !
          >r 
          2dup N @ commit_reveal >r
               N @ commit_reveal r> 
          == not or_die
	  swap tuck r@ 
	  verify_sig or_die r>
          verify_sig or_die;

   : a B ! Pub ! 
dup integer 0 == if 
   integer 0 T ! drop
else
   dup integer 2 == if drop
     dup tuck Pub @ verify_sig or_die 
     integer 1337 commit_reveal
     integer 1 == or_die ( only counting votes in same direction )
     B @ T ! 
   else
      integer 1 == or_die
      Pub @ integer 1337 double_signed_slash
      integer 0 T !
      integer 0 B !
      integer 1 Nonce +! 
   then
then B @ Bottom +! T @ Top +! ;
macro EPub binary ">>/binary, EPub/binary, <<" ;
integer 0 Nonce !
EPub integer 2 a call
EPub integer 2 a call
EPub integer 3 a call
Bottom @ Top @ Nonce @
">>/binary >>),
    [1, 2, 5] = language:run(A, 20000),
    % verify_sig
    % commit_reveal integer 1337
    % function call integer 1 = not if crash else then
    % > 2/3
    ok.

%Concerning the bet nonce. 
%If a lot of validators double-sign, it is important.
%If your partner closes without providing evidence of double-signing, he can make it look like he won when he lost.
%If you provide more information, it should result in a higher bet-nonce, so you can slash him.

%Everyone commits to how they reported.
%Everyone reveals how they reported.
test() ->
    {Pub, Priv} = testnet_sign:new_key(),
    EPub = base64:encode(Pub),
    testF(EPub, Priv),
    testG(EPub, Priv),
    testH(EPub, Priv),
    testI(EPub, Priv),
    testJ(EPub, Priv),
    testK(EPub, Priv),
    success.
%success.

% we can use testI contract to remove power in the weighted multisig from any validators who double-sign.

% weighted multisig with merkle identifier. and the ability to ignore anyone who signs on non-identical hashes with identical merkle identifiers.
% the ability to punish people who sign against the majority.
% ability to punish participants who fail to reveal.
% ability to punish participants who reveal early.

%success.

    
%We want a script that uses verify_sig on a hash, and if it verifies, then it calls it. A script like this can be easily modified to do anything else that person wants.
%the function should put a random nonce into the stack and immediately drop it, that way the hash we called wont every be reused, so the signature can't be reused.

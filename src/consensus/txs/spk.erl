-module(spk).
-export([acc1/1,acc2/1,entropy/1,
	 bets/1,space_gas/1,time_gas/1,
	 new/9,delay/1,cid/1,amount/1, 
	 nonce/1]).
-record(spk, {acc1, acc2, entropy, 
	      bets, space_gas, time_gas, 
	      delay, cid, amount = 0, nonce = 0}).
%scriptpubkey is the name that Satoshi gave to this part of the transactions in bitcoin.
%This is where we hold the channel contracts. They are turing complete smart contracts.
%Besides the SPK, there is the ScriptSig. Both participants of the channel sign the SPK, only one signs the SS.

acc1(X) -> X#spk.acc1.
acc2(X) -> X#spk.acc2.
bets(X) -> X#spk.bets.
delay(X) -> X#spk.delay.
entropy(X) -> X#spk.entropy.
space_gas(X) -> X#spk.space_gas.
time_gas(X) -> X#spk.time_gas.
cid(X) -> X#spk.cid.
amount(X) -> X#spk.amount.
nonce(X) -> X#spk.nonce.


new(Acc1, Acc2, CID, Bets, SG, TG, Delay, Nonce, Entropy) ->
    %Entropy = channel_feeder:entropy(CID, [Acc1, Acc2]) + 1,
    #spk{acc1 = Acc1, acc2 = Acc2, entropy = Entropy,
	 bets = Bets, space_gas = SG, time_gas = TG,
	 delay = Delay, cid = CID, nonce = Nonce}.
    

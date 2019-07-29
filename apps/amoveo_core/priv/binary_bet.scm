
(define (finish A)
  (return 0 10 A));delay nonce amount
;the blockchain expects 3 integers as the result of the contract.
;delay is how much time for the other party to give counter-evidence. for this contract we never have to use counter-evidence, so we can set it to 0 and instantly allow us to get our money out once the result of the oracle is known.
;nonce is how much priority this contract is given, if different contracts are provided as evidence. For this contract we never have to use counter-evidence, so the nonce doesn't matter.
;A/10000 is the portion of the money in the channel that you get

(define (require Bool)
  (cond (Bool ());if bool is true, do nothing
        (true fail)));otherwise, the contract should fail.

(define (unpack_oracle_data pdv) ;given the full oracle data provided by the blockchain, produce the integer result of that oracle. There are 4 possible outputs: 0,1,2, or 3. 0 means it is still open. 1 means true. 2 means false. 3 means it was a bad question.
  (let (((version pd) (car@ pdv))
        ((MarketID2 pd) (car@ pd))
        (pd (car pd))
        ((T _) (split pd 32))
        ((_ Result) (split T 1))
        )
    ((require (= 5 version));5 is the version for storing oracles. This makes sure that we are reading oracle data, and not something else like account data or channel data.
     (require (= MarketID2 (@ MarketID)));make sure the oracle being read is the one that we wanted to bet on.
     (++ --AAAA Result))));switch from 1 byte binary to a integer representation.


;load 4 global variables to configure the contract
(forth MarketID !
       Expires !
       Guess !
       Price !)

(let ((BlockchainData ());load blockchain data as a local variable. It is provided by the blockchain when the contract is executed.
      (OracleData (car BlockchainData));we expect the blockchain data to be a list of only 1 thing. the oracle we are betting on.
      (oracle_result ;calculate the integer result of the oracle, 0,1,2, or 3.
       (unpack_oracle_data OracleData)) ;0 -> unresolved. 1->true, 2->false, 3->bad_question.
      (amount ;calculate how much money you get
       (cond ((= oracle_result 0);unresolved oracle
              ((require (> height (@ Expires)));expiration height has been exceeded
               (@ Price));undo the bet. we both get our money back.
              ((= oracle_result 3);oracle resolved to bad question
               (@ Price));undo the bet. we both get our money back.
              ((= oracle_result (@ Guess));you won
               10000);all money goes to you
              (true ;you lose
               0))));you get none of the money
      )
  (finish amount))

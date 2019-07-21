
(define (oracle_reader pdv) ;given the full oracle data provided by the blockchain, produce the integer result of that oracle. There are 4 possible outputs: 0,1,2, or 3. 0 means it is still open. 1 means true. 2 means false. 3 means it was a bad question.
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

(let ((OracleData ());load oracle_data as a local variable. It is provided by the blockchain when the contract is executed.
      (oracle_result (oracle_reader (car OracleData))));calculate the integer result of the oracle. 0,1,2, or 3. 0 -> unresolved. 1->true, 2->false, 3->bad_question.
  (cond ((= oracle_result 0);unresolved oracle
         ((require (> height (@ Expires)));expiration height has been exceeded
          (return 0 10 (@ Price)));give us our money back
         ((= oracle_result 3);bad question
          (return 0 10 (@ Price)));give us our money back
         ((= oracle_result (@ Guess));you win
          (return 0 10 10000));all money goes to you
         (true ;you lose
          (return 0 10 0)))));all money goes to them
         

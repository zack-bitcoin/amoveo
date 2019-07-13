(import (basics.scm))
(macro mil () 1000000)

;load the variables that are used to configure the market contract for one particular channel.

Pubkey ! Period ! MarketID ! MaxPrice ! Expires ! Height ! Direction ! car OracleData ! ContractMode ! ;these input variables are used like constants. We don't update the values in any of these variables.


 
(macro car! (L H T) ;grab the head of the list on the top of the stack, store it in variable named H. Store the rest of the list in T.
       (() L car@ T ! H !))
(macro split! (Binary Bytes H T);grab the first few bytes of a binary L, store in H. put the rest in T.
       (()
        (split Binary Bytes)
        H ! T !))
;(define die () (return 9999999 0 0))
(define die () (return 9999999 0 0));the contract closes in a way that takes forever to actually get your money out of the channel, and it has nonce 0, so any other contract would close at a higher nonce.
(define require (b);if T is false, then the contract fails. Otherwise, continue.
  (cond ((b ())
         (true (die)))))
(macro set! (a b) (! b a)); example: setting variable X to 25. (set! X 25)
;(define byte_to_int (x) (() --AAAA x ++))
(macro byte_to_int (x) (() --AAAA x ++))


(set! IMaxPrice (- 10000 (@ MaxPrice)));we use this value a lot, the contract is shorter if we just calculate it once instead of over and over.
(set! Direction2 (= (@ Direction) 2));this is the flag for whether we are betting on true or false


(define helper (pdv);given the full oracle data provided by the blockchain, produce the integer result of that oracle. There are 4 possible outputs: 0,1,2, or 3. 0 means it is still open. 1 means true. 2 means false. 3 means it was a bad question.
  (()
   (car! pdv version PD)
   (require (= 5 (@ version)));5 means it is oracle-type data
   (car! (@ PD) MarketID2 PD)
   (set! PD (car (@ PD)))
   (require (= (@ MarketID) (@ MarketID2)));this means that the oracle ID of the oracle we have looked up matches the oracle ID that we wanted to bet with.
   (split! (@ PD) 32 _ T)
   (split! (@ T) 1 Result _)
   (byte_to_int (@ Result))))

(macro bet (oracle_result)
     ;depending on the result of the oracle, return a different value. This is used to compute the quantity of veo that will get transfered.
  '(cond (((= (@ Direction) oracle_result) 10000);true or fales, and you won
         ((and (> oracle_result 0)
                (< oracle_result 3));true or false and you lost.
           0)
         (true (@ IMaxPrice)))));bad_question or unresolved.
(macro no_publish ()
  (return (@ Period)
          (/ height (@ Period))
          0))
(define extract (signed_price_declaration DeclaredHeight DeclaredPrice PortionMatched)
  (()
   (split! signed_price_declaration 40 data sig)
   (require (verify_sig (@ sig) (@ data) (@ Pubkey)))
   (split! (@ data) 4 DeclaredHeight R)
   (split! (@ R) 2 DeclaredPrice R)
   (set! DeclaredPrice (++ --AAA= (@ DeclaredPrice)))
   (split! (@ R) 2 PortionMatched MarketID2)
   (set! PortionMatched (++ --AAA= (@ PortionMatched)))
   (require (= (@ MarketID2) (@ MarketID)))
   (require (> (@ DeclaredHeight) (@ Height)))))

(define price_range (F)
  (/ (* 10000 F)
     (+ (@ MaxPrice)
        10000)))

(macro evidence ()
  (()
   signed_price_declaration !
   (extract (@ signed_price_declaration) DeclaredHeight _DeclaredPrice PortionMatched)
   (require (> (@ DeclaredHeight)
              (- height (@ Period))))
   (return 
    (- (@ Expires) height);delay
    (+ 1 (/ (@ DeclaredHeight);nonce
            (@ Period)))
    (price_range (@ IMaxPrice)))));amount
(define abs (a)
  (cond (((< a 0) (- 0 a))
         (true a))))
(macro contradictory_prices ()
  (()
   signed_price_declaration1 !
   signed_price_declaration2 !
   (extract (@ signed_price_declaration1) h1 p1 pm1)
   (extract (@ signed_price_declaration2) h2 p2 pm2)
   (require (< (abs (- (@ h1) (@ h2)))
              (/ (@ Period) 2)));the operator made 2 contradictory price declarations at too close to the same point in time.
   (require (or (not (= (@ p1) (@ p2)))
               (not (= (@ pm1) (@ pm2)))));either the price or portion matched of the price declarations needs to be different for them to be considered contradictory.
   (return 0;delay
           (* 2 (mil));nonce
           0)));amount
(define minus_zero (a b)
  (cond (((> a b) (- a b))
         (true 0))))

(define match_order ()
  (()
   signed_price_declaration !
   (extract (@ signed_price_declaration) declared_height price pm)
   (cond (((@ Direction2) (set! price
                                (- 10000 (@ price))))))
   (require (not (> (@ price)
                    (@ MaxPrice))));enforce that we wont pay a higher price than we had agreed to pay.
   (set! oracle_result
         (helper (@ OracleData)))
   (set! nonce
         (+ (minus_zero (@ Expires)
                        (@ declared_height))
            (cond (((@ oracle_result) 3);if oracle_result is not 0, then it is considered "true".
                   (true 1)))))
   (set! delay
         (cond (((@ oracle_result) 0);if oracle_result is non-zero
                (true (+ (@ Expires)
                         (minus_zero (@ Expires)
                                     height))))))
                 
   (set! amount (bet (@ oracle_result)))
   (set! amount2
         (cond (((= (@ MaxPrice) (@ price));our bet was only partly matched.
                 (/ (+ (* (@ pm) (@ amount))
                       (* (@ IMaxPrice)
                          (- 10000 (@ pm))))
                    10000))
                (true 
                 (+ (@ amount)
                    (- (@ MaxPrice)
                       (@ price)))))))
   (@ delay) (@ nonce) (price_range (@ amount2))))

(macro unmatched ()
  '(cond (((= 0 (helper (@ OracleData)))
          (return (+ 2000 (+ (@ Expires)
                             (@ Period)))
                  0
                  (price_range
                   (@ IMaxPrice))))
         (true (return (@ Period)
                       1
                       (price_range
                        (@ IMaxPrice)))))))

(macro main (Mode)
  '(cond (((= Mode 0) (no_publish));if the market fails to publish the current price, this is how you can punish them.
         ((= Mode 1) (match_order));your bet was matched in the market, and now the oracle has resolved. This is how you enforce that you receive the correct amount from the contract.
         ((= Mode 2) (contradictory_prices));if the market publishes different prices at too near to the same time.
         ((= Mode 3) (evidence));if someone falsely claims that a market has failed to publish prices, but it has been publishing prices, this is used for the market operator to prove that they have been publishing prices.
         ((= Mode 4) (unmatched));your bet never got matched in the market. You want to get your  money out.
         (true (die)))))

(main (@ ContractMode))


;print



(import (basics.scm))
(macro mil () 1000000)

;load the variables that are used to configure the market contract for one particular channel.

Pubkey ! Period ! MarketID ! MaxPrice ! Expires ! Height ! direction ! car oracle_data ! contract_mode !

(cond (((= (@ direction) 2);bet_amount is different depending on the direction.
        (! 0 bet_amount))
       (true
        (! 10000 bet_amount))))
;swap contract_mode ! oracle

;defining helper functions
(macro car! (L H T) ;grab the head of the list on the top of the stack, store it in variable named H. Store the rest of the list in T.
       (() L car@ T ! H !))
(macro split! (Binary Bytes H T);grab the first few bytes of a binary L, store in H. put the rest in T.
       (()
        (split Binary Bytes)
        H ! T !))
;(define die () (return 9999999 0 0))
(macro die () (return 9999999 0 0));the contract closes in a way that takes forever to actually get your money out of the channel, and it has nonce 0, so any other contract would close at a higher nonce.
(define or_die (T);if it is false, then die.
  (cond ((T ())
         (true (die)))))
(macro set! (A B) (! B A)); example: setting variable X to 25. (set! X 25)
(define byte_to_int (x) (() --AAAA x ++))
;(macro byte_to_int (x) (() --AAAA x ++))



(define helper (pdv);given the full oracle data provided by the blockchain, produce the integer result of that oracle. There are 4 possible outputs: 0,1,2, or 3. 0 means it is still open. 1 means true. 2 means false. 3 means it was a bad question.
  (()
   (car! pdv version PD)
   (or_die (= 5 (@ version)))
   (car! (@ PD) MarketID2 PD)
   (set! PD (car (@ PD)))
   (or_die (= (@ MarketID) (@ MarketID2)))
   (split! (@ PD) 32 _ T)
   (split! (@ T) 1 Result _)
   (byte_to_int (@ Result))))

(deflet bet (proofStructure MaxPrice bet_amount A B C)
  ((bet2 (helper proofStructure)))
  (()
   (cond (((= bet2 0) (() (set! A 1) (set! B 1)))
          (true (() (set! A 0) (set! B 3)))))
   (set! C
         (cond (((= bet2 1) bet_amount)
                ((= bet2 2) (- 10000 bet_amount))
                ((= bet2 3) (- 10000 MaxPrice))
                ((= bet2 0) (- 10000 MaxPrice))
                (true (die)))))))
(define no_publish (Period)
  (return Period
          (/ height Period)
          (price_range 0)))
;(define extract (spd DeclaredHeight DeclaredPrice PortionMatched)
(define extract (spd DeclaredHeight DP PM)
  (()
   (split! spd 40 Data Sig)
   (or_die (verify_sig (@ Sig) (@ Data) (@ Pubkey)))
   (split! (@ Data) 4 DeclaredHeight R)
   (split! (@ R) 2 DP R)
   (set! DP (++ --AAA= (@ DP)))
   (split! (@ R) 2 PM MarketID2)
   (set! PM (++ --AAA= (@ PM)))
   (or_die (= (@ MarketID2) (@ MarketID)))
   (or_die (> (@ DeclaredHeight) (@ Height)))))

(define price_range (F)
  (/ (* 10000 F) (+ (@ MaxPrice) 10000)))

(define evidence ()
  (()
   spd !
   (extract (@ spd) DeclaredHeight _DeclaredPrice PortionMatched)
   (or_die (> (@ DeclaredHeight)
              (- height (@ Period))))
   (set! nonce (+ 1 (/ (@ DeclaredHeight) (@ Period))))
   (set! delay (- (@ Expires) height))
   (set! amount (price_range (- 10000 (@ MaxPrice))))
   (return (@ delay) (@ nonce) (@ amount))))
(define abs (a)
  (cond (((< a 0) (- 0 a))
         (true a))))
(define contradictory_prices ()
  (()
   spd1 ! spd2 !
   (extract (@ spd1) h1 p1 pm1)
   (extract (@ spd2) h2 p2 pm2)
   (or_die (< (abs (- (@ h1) (@ h2)))
              (/ (@ Period) 2)))
   (or_die (or (not (= (@ p1) (@ p2)))
               (not (= (@ pm1) (@ pm2)))))
   (return 0 (* 2 (mil)) 0)))
(define check_size (price maxprice)
  (not (cond (((= (@ direction) 2)
               (< price (- 10000 maxprice)))
              (true (> price maxprice))))))
(define minus_zero (A B)
  (cond (((> A B) (- A B))
         (true 0))))

(define match_order ()
  (()
   spd !
   (extract (@ spd) h p pm)
   (or_die (check_size (@ p) (@ MaxPrice)))
   (bet (@ oracle_data) (@ MaxPrice) (@ bet_amount) delay nonce amount)
   (set! delay2
         (* (@ delay)
            (+ (@ Expires)
               (minus_zero (@ Expires)
                           height))))
   (set! new_nonce
         (+ (@ nonce)
            (minus_zero (@ Expires) (@ h))))
   (set! p2 (cond (((= (@ direction) 2)
                    (- 10000 (@ p)))
                   (true (@ p)))))
   (set! amount2
         (cond (((= (@ MaxPrice) (@ p2));our bet was only partly matched.
                 (+ (/ (* (@ PM) (@ amount))
                       10000)
                    (/ (* (- 10000 (@ MaxPrice))
                          (- 10000 (@ PM)))
                       10000)))
                (true 
                 (+ (@ amount)
                    (- (@ MaxPrice)
                       (@ p2)))))))
   (@ delay2) (@ new_nonce) (price_range (@ amount2))))

(define unmatched ()
  (cond (((= 0 (helper (@ oracle_data)))
          (return (+ 2000 (+ (@ Expires)
                             (@ Period)))
                  0
                  (price_range
                   (- 10000 (@ MaxPrice)))))
         (true (return (@ Period)
                       1
                       (price_range
                        (- 10000 (@ MaxPrice))))))))

(define main (Mode)
  (cond (((= Mode 0) (no_publish (@ Period)))
         ((= Mode 1) (match_order))
         ((= Mode 2) (contradictory_prices))
         ((= Mode 3) (evidence ))
         ((= Mode 4) (unmatched))
         (true Mode))))

;car ; drop the `0` which is a flag for which version of the contract to run.

                                        ;oracle_data !
(main (@ contract_mode))

;(@ Period)
;(return (@ Period) (/ (@ Period) height) 0)
;height
;(bet (@ oracle_data) (@ bet_amount) (@ MaxPrice) R1 R2 R3)
;(@ R1) (@ R2) (@ R3)

print




(var c_oracle_starts
     c_ul
     c_ll
     c_pubkey
     c_period
     c_oid_0
     c_oid_64
     c_max_price
     c_expires
     c_height
     i_max_price)
(define (die)
  (return 9999999 0 0))
(define (require b)
  (cond ((= b 0) (fail))
        (true ())))
(define (digit_to_bin D);for digits  0-9
  (let (((B _) (split (+ D 48) 3)))
    B))
;(= (digit_to_bin 5) "5")
;(forth print print )
;(return 0 0 0)
(define (oracle_id dqh question_hash N)
  (cond
   ((= N 0) dqh)
   (true
    (let ((question
           (++ (++ "scalar " question_hash)
               (++ " bit number "
                   (digit_to_bin N))))
          (question_hash2 (hash question))
          (serialized
           (++ (++ (@ c_oracle_starts) 0);c oracle starts 6 or 3??
               (++ 0 question_hash2))))
      (
       ;question
       ;(forth print drop)
       ;serialized
       ;(forth print drop)
       (hash serialized))))))
;(oracle_id (hash 1) 0)
;(forth print print)
;(return 0 0 0)
(define (helper pdv N) ;given the full oracle data provided by the blockchain, produce the integer result of that oracle. There are 4 possible outputs: 0,1,2, or 3. 0 means it is still open. 1 means true. 2 means false. 3 means it was a bad question.
  (let (((version pd0) (car@ pdv))
        ((MarketID2 pd) (car@ pd0))
        (pd2 (car pd)))
    (cond ((= pd2 0) 0);oracle never created
          (true
           (let
               (((T _) (split pd2 32))
                ((_ Result) (split T 1))
                (MarketID (oracle_id (@ c_oid_0) (@ c_oid_64) N)))
             (
              (require (= 5 version))
              ;MarketID MarketID2;marketid is double encoded base64
              ;(print)
              ;(forth drop drop)
              (require (= MarketID2 MarketID))
              ;(print)
              (++ --AAAA Result)))))));switch from 1 byte binary to a integer representation.
;(define (multi_helper_b Accumulator ProofList N);returns a list of integer results from each oracle.
;  0)
(define (multi_helper_b Accumulator ProofList N);returns a list of integer results from each oracle.
  (cond ((= ProofList nil) (reverse Accumulator))
        (true
         (() ProofList (forth print drop)
          (let (((H T) (car@ ProofList))
                (M (helper H N))
                (Accumulator2 (cons M Accumulator)))
             (multi_helper_b Accumulator2 T (+ N 1)))))))
(define (multi_helper PL)
  (multi_helper_b (nil) PL 0))
;(define (map a b) 0)
(define (map f l)
  (cond ((= l nil) nil)
        (true (cons ((car l)
                     (call (@ f)))
                    (recurse f (cdr l))))))
(define (fold f a l)
  (cond ((= l nil) a)
        (true (fold f (a (car l)
                       ;a
                       (call (@ f)))
                    (cdr l)))))
(define (bad_f X Y) (or X (= Y 3)))
(define (bad? L) (fold 'bad_f 0 L))
;(require (= 0 (bad_f 0 0)))
;(require (= 1 (bad_f 3 0)))
;(require (= 1 (bad_f 20 1)))
;(require (= 0 (bad? (tree 1 2 1 2))))
;(require (= 1 (bad? (tree 1 3 1 2))))
;(forth print print)
;(return 0 0 0)
(define (unresolved_f Y X) (and Y (= X 0)))
(define (unresolved? L) (fold 'unresolved_f 0 L))
;(= 0 (unresolved? (tree 1 3 1 2)))
;(= 1 (unresolved? (tree 1 3 0 2)))
(define (two2zerof X)
  (cond ((= 2 X) 0)
        (true 1)))
(define (two2zero L) (map 'two2zerof L))
;(two2zero (tree 2 2 2 1))
;(forth print drop)
;(require (= (tree 0 0 0 1) (two2zero (tree 2 2 2 1))))
(define (bin2intf A B)
  (+ (* A 2) B))
(define (bin2int L) (fold 'bin2intf 0 L))
;(= 7 (bin2int (tree 0 0 1 1 1)))
(define (min A B)
  (cond ((< A B) A)
        (true B)))
(define (max A B)
  (cond ((< A B) B)
        (true A)))


(define (extract signed_price_declaration)
  (let (((sig data) (split signed_price_declaration 40))
        ((R0 DeclaredHeight) ((split data 4)))
        ((R DeclaredPrice0) ((split R0 2)))
        (DeclaredPrice (++ --AAA= DeclaredPrice0))
        ((MarketID2 PortionMatched0) (split R 2))
        (PortionMatched ((++ --AAA= PortionMatched0)))
        )
    (
     (require (verify_sig sig data (@ c_pubkey)))
     (require (= (@ c_oid_0) MarketID2))
     (require (not (< DeclaredHeight (@ c_height))))
     DeclaredHeight DeclaredPrice PortionMatched
     )))
(define (price_range F)
  (/ (* 10000 F)
     (+ (@ c_max_price) 10000)))
(define (abs a)
  (cond ((< a 0) (- 0 a))
        (true a)))
(define (minus_zero a b)
  (cond ((> a b) (- a b))
        (true 0)))
(define (bet2 delay nonce amount spd_height spd_price spd_partial)
  (let ((delay2 (* delay
                   (+ (minus_zero (@ c_expires)
                                  spd_height)
                      (@ c_expires))))
        (nonce2 (+ nonce
                   (minus_zero (@ c_expires)
                               spd_height)))
        (nspd_price (- 10000 spd_price))
        (amount2 
         (cond ((= nspd_price (@ c_max_price));partially matched trade
                (66 (forth print drop)
                    (+ (/ (* spd_partial amount) 10000)
                       (/ (* (- 10000 (@ c_max_price))
                             (- 10000 spd_partial))
                          10000))))
               (true
                ( (@ c_max_price)
                  spd_price
                  (forth print drop drop)
                ((require (> (@ c_max_price)
                            spd_price))
                (+ amount;fully matched, you get a refund based on the difference between what you agreed to pay, and the actual price matched.
                   (- (@ c_max_price)
                      spd_price))))))))
    (()
     amount amount2 (forth print drop drop)
     (return delay2 nonce2 (price_range amount2)))))
(define (bet oracle_result spd_height spd_price spd_partial direction);oracle_result is a list of results from each oracle.
  ( (bin2int (two2zero oracle_result)) (forth print drop)
  (cond ((bad? oracle_result)
         (bet2 0 3 (- 10000 (@ c_max_price))
               spd_height spd_price spd_partial))
        ((unresolved? oracle_result)
         (bet2 1 1 (- 10000 (@ c_max_price))
               spd_height spd_price spd_partial))
        (true
         (let ((result (bin2int (two2zero oracle_result)))
               (r1 (- result (@ c_ll)))
               (oracle_amount (cond ((> r1 0) r1)
                                    (true 0)))
               (amount0 (/ (* 1024 oracle_amount)
                           (- (@ c_ul) (@ c_ll))))
               (amount1 (/ (* amount0 10000)
                           1023))
               (amount2 (min amount1 10000))
               (amount22 (- 10000 amount2))
               (amount3 (cond ((= direction 1) (- 10000 amount22))
                              (true (- 0 amount22)))))
           (
            result
            oracle_amount
            amount0
            amount3
            (forth print drop drop drop drop)
            (bet2 0 3 amount3
                  spd_height spd_price spd_partial)))))))
(define (evidence)
  (let ((spd ())
        ((DeclaredHeight _ PortionMatched) (extract spd)))
    ((require (> DeclaredHeight (- height (@ c_period))))
     (return (- (@ c_expires) height);delay
             (+ 1 (/ DeclaredHeight (@ c_period)));nonce
             (price_range (@ i_max_price));amount
             ))))
(define (contradictory_prices)
  (let (((spd1 spd2) ())
        ((h1 p1 pm1) (extract spd1))
        ((h2 p2 pm2) (extract spd2)))
    ((print)
     (require (< (abs (- h1 h2))
                 (/ (@ c_period) 2)))
     (print)
     (require (or (not (= p1 p2))
                  (not (= pm1 pm2))))
     (print)
     (return 0 ;delay
             2000000 ;nonce
             (price_range 0))))) ;amount
;is {0,1,10000000}
;should be {105, 999, 0}
(define (match_order OracleData Direction)
  (let (((spd) ())
        ((h p0 pm) (extract spd))
        (p (cond ((= Direction 2) (- 10000 p0))
                 (true p0)))
        (oracle_result (multi_helper OracleData)))
    ;(55 (forth print drop)
    (bet oracle_result h p0 pm Direction)))
(define (unmatched oracle_data)
  (cond ((unresolved? (multi_helper oracle_data))
          (return (+ 2000 (+ (@ c_expires) (@ c_period)))
                  0
                  (price_range (@ i_max_price))))
         (true (return (@ c_period) 1 (price_range (@ i_max_price))))))


(define (no_publish)
  (return (@ c_period) (/ height (@ c_period)) 0))
(forth c_oracle_starts !
       c_ll !
       c_ul !
       c_pubkey !
       c_period !
       c_oid_0 !
       c_oid_64 !
       c_max_price !
       c_expires !
       c_height ! )
(let (
      ((mode OracleData Direction) ())
      ;((Direction OracleData mode) ())
      ;(OracleData (car OracleData0))
      )
  (
   (set! i_max_price (- 10000 (@ c_max_price)))
   mode ;(@ c_oracle_starts)
   (forth print drop)
   (case mode
    (1 (match_order OracleData Direction))
    (4 (unmatched OracleData))
    (0 (no_publish))
    (2 (contradictory_prices))
    (3 (evidence))
    (else (die)))))





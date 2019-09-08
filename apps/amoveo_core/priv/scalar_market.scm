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
(define (require b)
  (cond ((not b) (fail))
        (true ())))
(define (digit_to_bin D);for digits  0-9
  (let (((B _) (split (+ D 48) 3)))
    B))
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
      (hash serialized)))))
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
                (MarketID
                 (oracle_id (@ c_oid_0)
                            (@ c_oid_64)
                            N)))
             ((require (= 5 version))
              (require (= MarketID2 MarketID))
              (++ --AAAA Result)))))));switch from 1 byte binary to a integer representation.
(define (multi_helper_b Accumulator ProofList N);returns a list of integer results from each oracle.
  (cond ((= ProofList nil) (reverse Accumulator))
        (true
         (let (((H T) (car@ ProofList))
               (M (helper H N))
               (Accumulator2
                (cons M Accumulator)))
           (recurse Accumulator2
                    T
                    (+ N 1))))))
(define (multi_helper PL)
  (multi_helper_b (nil) PL 0))
(define (map f l)
  (cond ((= l nil) nil)
        (true
         (cons ((car l)
                (call (@ f)))
               (recurse f (cdr l))))))
(define (fold f a l)
  (cond ((= l nil) a)
        (true (recurse f
                    (a (car l)
                       (call (@ f)))
                    (cdr l)))))
(define (bad_f X Y) (or X (= Y 3)))
;(define (bad_f X Y) (or X (not (- Y 3))))
(define (bad? L) (fold 'bad_f 0 L))
;(require (= 0 (bad_f 0 0)))
;(require (= 1 (bad_f 3 0)))
;(require (= 1 (bad_f 20 1)))
;(require (= 0 (bad? (tree 1 2 1 2))))
;(require (= 1 (bad? (tree 1 3 1 2))))
;(forth print print)
;(return 0 0 0)
;(define (unresolved_f Y X) (or Y (= X 0)))
(define (unresolved_f Y X) (or Y (not X)))
(define (unresolved? L) (fold 'unresolved_f 0 L))
;(= 0 (unresolved? (tree 1 3 1 2)))
;(= 1 (unresolved? (tree 1 3 0 2)))
(define (two2zerof X) (not (- X 1)))
;  (cond ((= 2 X) 0)
;        (true 1)))
(define (two2zero L) (map 'two2zerof L))
;(two2zero (tree 2 2 2 1))
;(forth print drop)
;(require (= (tree 0 0 0 1) (two2zero (tree 2 2 2 1))))
(define (bin2intf A B) (+ (* A 2) B))
(define (bin2int L) (fold 'bin2intf 0 L))
;(= 7 (bin2int (tree 0 0 1 1 1)))
(define (min A B)
  (cond ((< A B) A)
        (true B)))
(define (max A B)
  (cond ((< A B) B)
        (true A)))
(define (extract signed_price_declaration)
  (let (((sig data)
         (split signed_price_declaration 40))
        ((R0 DeclaredHeight) ((split data 4)))
        ((R DeclaredPrice0) ((split R0 2)))
        (DeclaredPrice (++ --AAA= DeclaredPrice0))
        ((MarketID2 PortionMatched0) (split R 2))
        (PortionMatched
         ((++ --AAA= PortionMatched0))))
    ((require (verify_sig sig data (@ c_pubkey)))
     (require (= (@ c_oid_0) MarketID2))
     (require (not (< DeclaredHeight
                      (@ c_height))))
     DeclaredHeight DeclaredPrice PortionMatched)))
(define (price_range F)
  (/ (* 10000 F)
     (+ (@ c_max_price) 10000)))
(define (abs a)
  (cond ((< a 0) (- 0 a))
        (true a)))
(define (evidence)
  (let ((spd ())
        ((DeclaredHeight _ PortionMatched)
         (extract spd)))
    ((require (> DeclaredHeight
                 (- height (@ c_period))))
     (return (- (@ c_expires) height);delay
             (+ 1 (/ DeclaredHeight
                     (@ c_period)));nonce
             (price_range (@ i_max_price));amount
             ))))
(define (contradictory_prices)
  (let (((spd1 spd2) ())
        ((h1 p1 pm1) (extract spd1))
        ((h2 p2 pm2) (extract spd2)))
    ((require (< (abs (- h1 h2))
                 (/ (@ c_period) 2)))
     (require (or (not (= p1 p2))
                  (not (= pm1 pm2))))
     (return 0 ;delay
             2000000 ;nonce
             (price_range 0))))) ;amount
(define (enforce_price_limit direction spd_price)
  (cond (direction
         (< (@ i_max_price)
            spd_price))
        (true (> (@ c_max_price)
                 spd_price))))
(define (match_amount oracle_result direction)
  (cond
   ((bad? oracle_result)
    (@ i_max_price))
   ((unresolved? oracle_result)
    ((require (> height (@ c_expires)))
;terminates contract execution here.
     (return 0;delay
             (@ c_expires);nonce
             (price_range
              (@ i_max_price)))));amount
   (true
    (let ((integer_result
           (bin2int (two2zero
                     oracle_result)))
 ;a linear mapping from [c_ll, c_ul] -> [0, 10000]
;this is doing the leverage, and switching from 10-bit format to [0,10000] format all in one step
          (scaled (/ (* 10000
                        (- integer_result
                           (@ c_ll)));lower limit
                     (- (@ c_ul);upper limit
                        (@ c_ll))))
          (bounded
           (max 0 (min scaled 10000)))
          (result (- bounded
                     (cond (direction 10000)
                           (true 0)))))
      result))))
(define (match_order OracleData direction)
  (let (((spd) ())
        ((spd_height spd_price spd_partial)
         (extract spd))
        (oracle_result (multi_helper OracleData))
        (amount
         (match_amount oracle_result
                       direction))
        (nonce2 (+ 3 (max 0 (- (@ c_expires)
                               spd_height))))
        (spd_price2
         (cond
          (direction (- 10000 spd_price))
          (true spd_price)))
        (amount2 
         (cond ((= spd_price2
                   (@ c_max_price))
                ;partially matched trade
                (+ (/ (* spd_partial amount) 10000)
                   (/ (* (- 10000 (@ c_max_price))
                         (- 10000 spd_partial))
                      10000)))
               (true
;fully matched, you get a refund based on
;the difference between what you agreed to
;pay, and the actual price matched.
                ((require (enforce_price_limit
                           direction
                           spd_price))
                 (+ amount
                    (- (@ c_max_price)
                       spd_price2)))))))
    (return 0 nonce2 (price_range amount2))))

(define (unmatched oracle_data)
  (cond ((unresolved? (multi_helper oracle_data))
         (return (+ 2000 (+ (@ c_expires)
                            (@ c_period)))
                  0
                  (price_range (@ i_max_price))))
        (true (return (@ c_period);delay
                      1;nonce
                      (price_range
                       (@ i_max_price))))));amount
(define (no_publish)
   (return (@ c_period);delay
           (/ height (@ c_period));nonce
           0));amount
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
(let (((mode OracleData Direction) ()))
  ((set! i_max_price (- 10000 (@ c_max_price)))
   (case mode
    (1 (match_order OracleData (- Direction 1)))
    (4 (unmatched OracleData))
    (0 (no_publish))
    (2 (contradictory_prices))
    (3 (evidence))
    (else (fail)))))

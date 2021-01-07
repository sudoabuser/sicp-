# sicp-

__*EXERCISE 1.1.8*__

(define (cbrt-iter guess x)
(if (good-enough? guess x)
       guess
       (cbrt-iter (improve guess x)x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.000000001))

(define (cube x) (* x x x)) 

(define (improve guess x)
  (/ (+ (/ x (square guess) ) (* 2 guess) ) 3 ))

(define (square x) ( * x x))

(define (kupkok x)
  (cbrt-iter 1.0 x))



__computing iterative factorial__
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* product counter) (+ 1 counter) max-count)))
      
__computing recursive factorial__
(define (factorial n)
  (cond ((= n 1) 1)
        ((= n 0) 1)
        (else (* n (factorial (- n 1))))))
__some iterations of recursive count-change function *not completed*__
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
(cond ((= amount 0) 1)
((or (< amount 0) (= kinds-of-coins 0)) 0)
(else (+ (cc amount
(- kinds-of-coins 1))
(cc (- amount
(first-denomination
kinds-of-coins))
kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
(cond ((= kinds-of-coins 1) 1)
((= kinds-of-coins 2) 5)
((= kinds-of-coins 3) 10)
((= kinds-of-coins 4) 25)
((= kinds-of-coins 5) 50)))

(cc 100 5)
 (+ (cc 100 4)
    (+ (cc 100 3)
       (+ (cc 100 2)
          (+ (cc 100 1)
             (+ 0
               1)))))
             
             (cc 95 2)
             (+ (cc 95 1)
                
          (cc 90 3)
       (cc 75 4)
    
    (cc 50 5)
    
    
    
*Exercise 1.11:* 
__recursive__

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* (f (- n 2)) 2) (* (f (- n 3)) 3))))
__iterative__
--?
*Exercise 1.12:*
(define (pascal row column)
  (cond ((= row column) 1)
        ((< row column) 0)
        ((< column 0) 0)
        ((< row 0) 0)
        ((= column 1) 1)
        (else (+ (pascal (- row 1) (- column 1)) (pascal (- row 1) column)))))
        
*Exercise 1.21:*
(define (smallest-divisor n counter)
  (cond ((= counter n) counter)
      ((= (remainder n counter) 0) counter)
      (else (smallest-divisor n (+ 1 counter)))))
(define (square x) (* x x))
(define (ekok n) (smallest-divisor n 2))
  
 
 *Exercise 1.23:*
 (define (runtime)
  (current-milliseconds))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next x)
  (cond ((= x 2)(+ 1 x))
         (else (+ 2 x)))) 

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))
  
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
  
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
      
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

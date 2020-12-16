# sicp-

*EXERCISE 1.h1.8*

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



__ computing iterative factorial __
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* product counter) (+ 1 counter) max-count)))
      
__ computing recursive factorial __
(define (factorial n)
  (cond ((= n 1) 1)
        ((= n 0) 1)
        (else (* n (factorial (- n 1))))))

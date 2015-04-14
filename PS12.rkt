; Alex Agudelo & Yunfan Zhang
; Problem Set 12

;************************************************************
; Problem 1

; Purpose: 
; Consumes a non-empty string
; and constructs a palindrome by mirring
; the string around the last letter
; String -> String
(check-expect (make-palindrome "fundies") "fundieseidnuf")
(check-expect (make-palindrome "them") "themeht")
(check-expect (make-palindrome "Alex") "AlexelA")
(define (make-palindrome str)
  (local (; [List-of String] [List-of String] -> [List-of String]
          ; adds the str to another str using an accumulator
          ; creates the palindrome
          (define (pal-a LoS0 acc)
            (cond
              [(empty? LoS0) acc]
              [(cons? LoS0)
               (pal-a (rest LoS0) (cons (first LoS0) acc))])))
    (implode (pal-a (rest (reverse (explode str))) (reverse (explode str))))))

; Purpose:
; consumes a string and determines is it is a palindrome
; String -> Boolean
(check-expect (is-palindrome? "hih") true)
(check-expect (is-palindrome? "hiih") true)
(check-expect (is-palindrome? "themeht") true)
(check-expect (is-palindrome? "fundieseidnuf") true)
(check-expect (is-palindrome? "makeekke") false)
(define (is-palindrome? str)
  (is-palindrome-base (explode str) (reverse (explode str))))

(define (is-palindrome-base LoS1 LoS2)
  (cond
    [(and (empty? LoS1) (empty? LoS2))
     true]
    [(string=? (first LoS1) (first LoS2))
     (is-palindrome-base (rest LoS1) (rest LoS2))]
    [else
     false]))

;**********************************************************************************
; Problem 2

; Purpose:
; Consumes a Natural Number and returns if the number is prime or not
; NaturalNumber -> Boolean
(check-expect (prime? 5) true)
(check-expect (prime? 8) false)
(check-expect (prime? 21) false)
(check-expect (prime? 2) true)
(check-expect (prime? 1) true)
(check-expect (prime? 19) true)
(define (prime? n)
  (prime-base n 2))
; Purpose:
; Same as prime function excepts it takes in a counter argument
; to keep track of the current factor 
; NaturalNumber Naturalnumber -> Boolean
(check-expect (prime-base 5 2) true)
(check-expect (prime-base 9 2) false)
(check-expect (prime-base 13 2) true)
(define (prime-base n x)
  (cond
    [(> x (sqrt n))
     true]
    [(= (modulo n x) 0)
     false]    
    [else
     (prime-base n (add1 x))]))

; Purpose:
; Consumes a Natural Number and
; produces the list of prime numbers up to n
(check-expect (list-primes 2) (list 1))
(check-expect (list-primes 4) (list 1 2 3))
(check-expect (list-primes 20) (list 1 2 3 5 7 11 13 17 19))
(define (list-primes n)
  (local (; add elements to the accumulator
          ; if it is odd
          ; NaturalNumber [List-of Number] -> [List-of Number]
          (define (prime-accum x acc)
            (cond
              [(= 0 x) acc]
              [(prime? x)
               (prime-accum (sub1 x) (cons x acc))]
              [else
               (prime-accum (sub1 x) acc)])))
    (prime-accum (sub1 n) '())))

;************************************************************************
;Problem 3



  
    
      
      

; Alex Agudelo & Yunfan Zhang
; Problem Set 12

;************************************************************
; Problem 1

; Data Definition
; NN is Natural Number

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
; NN NN -> Boolean
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
; NN -> [List-of Number]
(check-expect (list-primes 2) (list 1))
(check-expect (list-primes 4) (list 1 2 3))
(check-expect (list-primes 20) (list 1 2 3 5 7 11 13 17 19))
(define (list-primes n)
  (local (; add elements to the accumulator
          ; if it is odd
          ; NN [List-of Number] -> [List-of Number]
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

; Purpose:
; produces the fibonacci number of the input number
; without accumulators
; NaturalNumber -> Number
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 2) 1)
(check-expect (fibonacci 11) 89)
(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else 
     (+ (fibonacci (- n 1))
        (fibonacci (- n 2)))]))

; Purpose:
; Rewrite the fibonacci series with accumulators
; NN -> Number
(check-expect (fibAcc 0) 0)
(check-expect (fibAcc 1) 1)
(check-expect (fibAcc 2) 1)
(check-expect (fibAcc 11) 89)
(define (fibAcc n)
  (local (; use an accumulator to 
          ; add the fib numbers
          ; NN NN NN -> Number
          (define (fibAccFunc x fn fn-1)
            (cond
              [(= x n) fn]
              [(= x 0)
               (fibAccFunc (add1 x) 1 0)]            
              [else
               (fibAccFunc (add1 x) (+ fn fn-1) fn)])))
    (fibAccFunc 0 0 0)))

; Purpose:
; Conumes a NN and produces
; a list of Fibonacci numbers from F0 to Fn
(check-expect (list-fibonacci 0) '(0))
(check-expect (list-fibonacci 4) '(0 1 1 2 3))
(check-expect (list-fibonacci 7) '(0 1 1 2 3 5 8 13))

(define (list-fibonacci n)
  (local (; takes the original function
          ; and adds a counter to the arguments
          ; to keep track of the current iteration
          ; It also adds accumulator to store the current
          ; value
          ; Number [list-of Number] -> Number
          (define (list-fib-acc x acc)
            (cond
              [(< x 0) acc]
              [else
               (list-fib-acc (sub1 x) (cons (fibAcc x) acc))])))
    (list-fib-acc n '())))

;************************************************************************
; Problem 4

; Trick is one of:
;   - (list Card Card Card Card ...) // any number of cards, for the problem 
; it is a 4 card hand, but in reality it can be any number of cards

; Suite is one of:
;    - 'spade
;    - 'heart
;    - 'club
;    - 'diamond

(define-struct card [suite value])
; (make-card Symbol Number)
; Suite is a Symbol:
; Interpretatino: the suite of the card
; Value is a Number:
; Interpretation: The number value of the card

#;(define (card-tmpl a-C)
    ... (card-suite a-C) ...
    ... (card-value a-C) ...)

(define-struct player [name numWins])
; (make-player Number)
; name is a String:
; Name of the player
; numWins is a number:
; Interpretation: The number of wins the player has

#;(define (player-tmpl a-p)
    ... (player-name a-p) ...
    ... (player-numWins a-p) ...)

(define-struct game [players tricks])
; (make-game [list-of Player] [List-of Trick])
; players is a [List-of Player]
; Interpreation: All the players in the game
; tricks is a [List-of Tricks]
; Interpretation: All the tricks in the game

#;(define (game-tmpl a-g)
    ... (game-players a-g) ...
    ... (game-tricks a-g) ...)

(define player1 (make-player "Alex" 0))
(define player2 (make-player "Ben" 0))
(define player3 (make-player "Charile" 0))
(define player4 (make-player "David" 0))
(define card1 (make-card 'heart 4))
(define card2 (make-card 'spade 5))
(define card22 (make-card 'spade 8))
(define card33 (make-card 'diamond 8))
(define card3 (make-card 'diamond 2))
(define card4 (make-card 'club 5))
(define card44 (make-card 'club 7))
(define trick1 (list card2 card3 card4))
(define trick2 (list card1 card4 card3))
(define trick3 (list card3 card4))
(define trick4 (list card1 card4))
(define trick5 (list card4 card4))
(define trick6 (list card22 card33 card2))
(define trick7 (list card4 card2 card22))
(define trick8 (list card1 card2 card3 card4))
(define trick9 (list card4 card4 card4 card2))
; Purpose:
; Given two cards, see if the first card 
; evaluates higher then the second card
; if the first card is higher, then return true
; else false
; Card Card ->  Boolean
(check-expect (cardSuite card1 card2) false)
(check-expect (cardSuite card2 card1) true)
(check-expect (cardSuite card1 card3) true)
(check-expect (cardSuite card3 card4) true)
(check-expect (cardSuite card3 card2) false)
(check-expect (cardSuite card4 card33) false)
(check-expect (cardSuite card4 card2) false)
(check-expect (cardSuite card2 card4) true)

(define (cardSuite a-c a-c2)
  (cond
    [(symbol=? (card-suite a-c) 'spade)
     true]
    [(symbol=? (card-suite a-c2) 'spade)
     false]
    [(symbol=? (card-suite a-c) 'heart)
     true]
    [(symbol=? (card-suite a-c) 'diamond)
     (cond
       [(symbol=? (card-suite a-c2) 'heart)
        false]
       [else true])]
    [else false]))

; Purpose:
; Determines if two cards are equal
; Card Card -> Boolean
(check-expect (card=? card1 card1) true)
(check-expect (card=? card2 card2) true)
(check-expect (card=? card2 card1) false)
(check-expect (card=? card4 card1) false)
(define (card=? a-c1 a-c2)
  (cond
    [(and (symbol=? (card-suite a-c1) (card-suite a-c2))
          (= (card-value a-c1) (card-value a-c2)))
     true]
    [else
     false]))
; Purpose:
; Given the winning card and a trick return the number player
; that won. x is the current player which starts at 1 always
; Card Trick Number -> Number
(check-expect (cardWinner card2 trick1 1) 1)
(check-expect (cardWinner card3 trick1 1) 2)
(check-expect (cardWinner card4 trick1 1) 3)
(check-error (cardWinner card1 trick1 1) "No Cards")
(check-expect (cardWinner card4 trick8 1) 4)
(define (cardWinner a-c a-t x)
  (cond
    [(empty? a-t) (error "No Cards")]
    [(card=? a-c (first a-t))
     x]
    [else
     (cardWinner a-c (rest a-t) (add1 x))]))
; Purpose:
; takes a trick and returns the card
; that won
; Trick -> Card
(check-expect (trickWinner trick1) card2)
(check-expect (trickWinner trick2) card4)
(check-expect (trickWinner trick3) card4)
(check-expect (trickWinner trick6) card22)
(check-expect (trickWinner trick7) card22)
(check-expect (trickWinner trick9) card2)
(define (trickWinner trick)
  (local (; Tests the current card
          ; against all the remaining card
          ; and if it returns true it is the winner
          ; Card Trick -> Boolean
          (define (cardWinner card trick-rem)
            (cond
              [(empty? trick-rem)
               true]
              [(> (card-value card) (card-value (first trick-rem)))
               (cardWinner card (rest trick-rem))]
              [(= (card-value card) (card-value (first trick-rem)))
               (cond
                 [(cardSuite card (first trick-rem)) 
                  (cardWinner card (rest trick-rem))]
                 [else
                  false])]
              [else 
               false])))
    (cond
      [(empty? (rest trick))
       (first trick)]
      [(cardWinner (first trick) (rest trick))
       (first trick)]
      [else (trickWinner (rest trick))])))

; Purpose:
; Returns the player with the most wins
; game -> Player
(define gameWin1 (make-game (list (make-player "Blah" 1)
                                 (make-player "Moe" 2)) empty))
(define gameWin2 (make-game (list (make-player "blah" 2)
                                  (make-player "Moore" 4)
                                  (make-player "Tim" 5)
                                  (make-player "John" 2)) empty))
(check-expect (mostWins gameWin1) (make-player "Moe" 2))
(check-expect (mostWins gameWin2) (make-player "Tim" 5))
(define (mostWins a-g)
  (local (; takes in the list of players
          ; from the game and produces
          ; the player with the most wins
          ; [List-of Player] -> [List-of Player]
          (define (mostWins-base LoP)
            (cond
              [(empty? (rest LoP))
               (first LoP)]
              [(> (player-numWins (first LoP)) (player-numWins (second LoP)))
               (mostWins-base (cons (first LoP) (rest (rest LoP))))]
              [else
               (mostWins-base (cons (second LoP) (rest (rest LoP))))])))
   (mostWins-base (game-players a-g))))

; Purpose:
; Given which player won, update that 
; corresponding player with an additional win
; it also removes the trick that was just used
; so the game incremements to the next trick
; Number Game -> [List-of Player]
(define gameUpdate1 (make-game (list (make-player "Blah" 1)
                                     (make-player "Moe" 2)) (list trick1)))
(define gameUpdate2 (make-game (list (make-player "Blah" 1)
                                     (make-player "Moe" 2)
                                     (make-player "Tim" 3)
                                     (make-player "David" 4)) (list trick1)))
(check-expect (updateWin 2 gameUpdate1) (make-game
                                         (list (make-player "Blah" 1)
                                               (make-player "Moe" 3)) empty))
(check-expect (updateWin 1 gameUpdate1) (make-game
                                         (list (make-player "Blah" 2)
                                               (make-player "Moe" 2)) empty))
(check-expect (updateWin 3 gameUpdate2) (make-game 
                                         (list (make-player "Blah" 1)
                                               (make-player "Moe" 2)
                                               (make-player "Tim" 4)
                                               (make-player "David" 4)) empty))
(check-expect (updateWin 4 gameUpdate2) (make-game 
                                         (list (make-player "Blah" 1)
                                               (make-player "Moe" 2)
                                               (make-player "Tim" 3)
                                               (make-player "David" 5)) empty))
(define (updateWin n a-g)
  (local (; Create a new function with
          ; a counter iterator
          ; Number [List-of Player] -> game
          (define (updateWin-base x LoP)
            (cond
              [(= x n)
               (cons (make-player (player-name (first LoP)) (add1 (player-numWins (first LoP)))) (rest LoP))]
              [else
               (cons (first LoP) (updateWin-base (add1 x) (rest LoP)))])))
    (make-game (updateWin-base 1 (game-players a-g)) (rest (game-tricks a-g)))))


; Purpose:
; takes a game which is a series of tricks 
; and determines which player wins the game
; [List-of Tricks] -> Player
(define game1 (make-game (list player1 player2 player3 player4) 
                         (list trick1 trick1)))
(define game2 (make-game (list player1 player2 player3 player4)
                         (list trick1 trick2 trick3 trick4)))
(define game3 (make-game (list player1 player2 player3 player4) 
                         (list trick1 trick2 trick3 trick4 trick5 trick6 trick7)))
(define game4 (make-game (list player1 player2 player3 player4)
                         (list trick9 trick9 trick9)))
(check-expect (winner game1) (make-player "Alex" 2))
(check-expect (winner game2) (make-player "Ben" 3))
(check-expect (winner game3) (make-player "Ben" 3))
(check-expect (winner game4) (make-player "David" 3))
(define (winner a-g)
  (cond
    [(empty? (game-tricks a-g)) (mostWins a-g)]
    [else
     (cond
       [(= (cardWinner (trickWinner (first (game-tricks a-g)))
                       (first (game-tricks a-g)) 1) 1)
        (winner (updateWin 1 a-g))]
       [(= (cardWinner (trickWinner (first (game-tricks a-g)))
                       (first (game-tricks a-g)) 1) 2)
        (winner (updateWin 2 a-g))]
       [(= (cardWinner (trickWinner (first (game-tricks a-g)))
                       (first (game-tricks a-g)) 1) 3)
        (winner (updateWin 3 a-g))]
       [(= (cardWinner (trickWinner (first (game-tricks a-g)))
                       (first (game-tricks a-g)) 1) 4)
        (winner (updateWin 4 a-g))])]))

; ****************************************************************************************************
               
;;Problem 5
;;Data definition
;;A twitter is a structure (make-user string [List-of String])
(define-struct user (handle tweeps))
; Handle is a string
; Interpretation: name of the user
; Tweeps is a [List-of String]
; Interpretation: Names of the follows of the user
#;(define (user-tmpl a-u)
    ... (user-handle a-u) ...
    ... (user-tweeps a-u) ...)

(define user1 (make-user "Alice" (list "Bob" "Charles" "Dave")))
(define user2 (make-user "Bob" (list "Alice" "Charles")))
(define user3 (make-user "Charles" (list "Bob" "Dave")))
(define user4 (make-user "Dave" (list "Alice" "Charles")))
(define user5 (make-user "Echo" empty))
(define user6 (make-user "Fox" (list "BoB" "James" "Charles" "Dave" "Echo")))

;;A twitter-network is one of:
;; - empty
;; - [List-of User]
(define twi-nwk1 (list user1 user2 user3 user4 user5))
(define twi-nwk2 (list user1 user2 user3 user5 user6))
(define twi-nwk3 (list user1 user3 user5))

; Purpose:
; produces a list of all of the handles in the network
;;list-handles: Twitter-network -> [List-of String]
(define (list-handles nwk)
  (local [;plus: [List-of String] [List-of String] -> [List-of String] 
          (define (plus nwk acc)
            (cond [(empty? nwk) acc]
                  [else (cons (user-handle (first nwk)) (plus (rest nwk) acc))]))] 
    (cond [(empty? nwk) nwk]
          [else (plus nwk empty)])))

(check-expect (list-handles twi-nwk1) (list "Alice" "Bob" "Charles" "Dave" "Echo"))
(check-expect (list-handles empty) empty)

; Purpose
; Given a network, produce the handle that 
; has the most followers
;;most-followers: Twitter-network -> String
(check-expect (most-followers twi-nwk1) user1)
(check-expect (most-followers twi-nwk2) user6)
(define (most-followers nwk)
  (local (; accumlator of the amount of followers 
          ; for a single user
          ; User Number -> Number
          (define (user-accum a-u acc)
            (cond
              [(empty? (user-tweeps a-u)) acc]
              [else
               (user-accum (make-user (user-handle a-u) (rest (user-tweeps a-u))) (add1 acc))]))
          ; Returns the user with the most followers
          ; Network User -> User
          (define (nwk-accum nwk1 acc)
            (cond
              [(empty? nwk1) acc]
              [(> (user-accum (first nwk1) 0) (user-accum acc 0))
               (nwk-accum (rest nwk1) (first nwk1))]
              [else
               (nwk-accum (rest nwk1) acc)])))
    (nwk-accum nwk (first nwk)))) 


; Purpose:
; Takes a follow and determines if there is 
; a corresponding person in the network
; String nwk -> maybe
; Maybe is one of:
;   - User
;   - false
(check-expect (corrFollower "Alice" twi-nwk1) user1)
(check-expect (corrFollower "George" twi-nwk1) false)
(check-expect (corrFollower "Dave" twi-nwk1) user4)
(define (corrFollower str nwk)
  (cond
    [(empty? nwk) false]
    [(string=? str (user-handle (first nwk)))
     (first nwk)]
    [else
     (corrFollower str (rest nwk))]))

; Purpose:
; Given the original person, see if the 
; corresponding person is a follower of the 
; original person
; User User -> Boolean
(check-expect (origFollower user1 user2) true)
(check-expect (origFollower user2 user3) true)
(check-expect (origFollower user4 user5) false)
(check-expect (origFollower user5 user6) true)
(define (origFollower a-u follower)
  (cond
    [(empty? (user-tweeps follower)) false]
    [(string=? (first (user-tweeps follower)) (user-handle a-u))
     true]
    [else
     (origFollower a-u (make-user (user-handle follower) (rest (user-tweeps follower))))]))

; Purpose:
; Consumes a network and determines whether it 
; contains two users who follow each other
; Network -> Boolean
(check-expect (friends? twi-nwk1) true)
(check-expect (friends? twi-nwk2) true)
(check-expect (friends? twi-nwk3) false)
(define (friends? nwk)
  (local (; determines if the first person in the network
          ; has a friend
          ; User nwk -> Boolean
          (define (hasFriendInNetwork? a-u nwk1)
            (cond
              [(empty? nwk1) false]
              [(user? (corrFollower (first (user-tweeps a-u)) nwk1))
               (origFollower a-u (corrFollower (first (user-tweeps a-u)) nwk1))]
              [(boolean? (corrFollower (first (user-tweeps a-u)) nwk1))
               (hasFriendInNetwork? (make-user (user-handle a-u) (rest (user-tweeps a-u))) (rest nwk1))])))
    (cond
      [(empty? nwk) false]
      [(hasFriendInNetwork? (first nwk) (rest nwk))
       true]
      [else
       (friends? (rest nwk))])))
               




  
    
      
      

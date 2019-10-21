;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname gameLogic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;Function that searches a specific index in a list
;;start: where does the list to analyze starts
;;index: index of the item to be search
;;list: list where the item is going to be search
;;return: element in that index, if the element is not there then it returns -1
(define (getElementByIndex start index list1)
  (cond((null? list1) -1)
       ((equal? start index) (car list1))
       (else(getElementByIndex (+ start 1) index (cdr list1)))))

;;Function that eliminates an element from the list
;;element: Element to be removed
;;list1: List where it is going to be remove
;;return: list without said element
(define (eliminateFromList element list1)
  (cond((null? list1) list1)
       ((equal? element (car list1)) (eliminateFromList element (cdr list1)))
       (else (cons (car list1) (eliminateFromList element (cdr list1))))))
(eliminateFromList 2 (list 1 4 3))

;;Eliminates a element from the list based on its index
;;list1: list to be analyze
;;index: index chosen to eliminate
;;curren: index of the current process
;;return: list without the chosen element
(define (eliminateFromListByIndex list1 index currentIndex)
  (cond((null? list1) list1)
       ((= index currentIndex) (eliminateFromListByIndex (cdr list1) index (+ currentIndex 1)))
       (else(cons (car list1) (eliminateFromListByIndex (cdr list1) index (+ currentIndex 1))))))

;;Function that get the length of a list
;;list1: List to be analyze
;;return: List length
(define (listLength list1)
  (cond((null? list1) 0)
       (else(+ 1 (listLength (cdr list1))))))

;;Function that says whether a player has a natural BlackJack or not
;;list1: List of carts the player has
;; total: total amount of the value of the cards
;;return: Whether the players has a natural blackjack
(define (isNaturalBlackJack list1 total)
  (cond ((null? list1)
         (cond((= 21 total) #t)
              (else #f)))
        (else(isNaturalBlackJack (cdr list1) (+ total (caar list1))))))
(isNaturalBlackJack (list '(10) '(11)) 0)

;;Auxilaire function for shuffling the deck
;;list1: deck
;;theChosenOne: random index chosen on previous function
;;return: shuffled deck
(define (shuffleDeckAux list1 theChosenOne)
  (cons (getElementByIndex 0 theChosenOne list1) (shuffleDeck (eliminateFromListByIndex list1 theChosenOne 0))))

;;Function that suffles a given deck
;;deck: deck to be suffle
;;return: suffled deck
(define (shuffleDeck deck)
  (cond((null? deck) deck)
       (else(shuffleDeckAux deck (random (listLength deck))))))

(shuffleDeck (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))

;;Function sums all the card values without counting As
;;list1: cards to be analyze
;;sum: current sum of the cards
;;return: the sum without As
(define (sumWithoutA list1 sum)
  (cond((null? list1) sum)
       ((equal? (cadar list1)  'A) (sumWithoutA (cdr list1) sum))
       (else(sumWithoutA (cdr list1) (+ (caar list1) sum)))))

(sumWithoutA (list (list 10 'A) (list 5 'B) (list 11 'A)) 0)

;;Auxiliare function for getting random card out of the deck
;;theChosenOne: random index
;;deck: deck to be processed
;;return: a list with the random selected card on front
(define (getRandomCardFromDeckAux theChosenOne deck)
  (cons (getElementByIndex 0 theChosenOne deck) (eliminateFromListByIndex deck theChosenOne 0)))

;;Function for getting a random card out of the deck
;;deck: deck to be processed
;;return: a list with the random selected card on front 
(define (getRandomCardFromDeck deck)
  (getRandomCardFromDeckAux (random (listLength deck)) deck))

(getRandomCardFromDeck (list 1 2 3 4 5 6 7 8 9))



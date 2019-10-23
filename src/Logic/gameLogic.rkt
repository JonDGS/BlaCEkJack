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
       ((> 0 index) (getElementByIndex start (+ (listLength list1) index) list1))
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

;;Function that makes a player selected the "stand" option
;;player: player to be analyze
;;return: whether the player stood or not
(define (isPlayerDone player)
  (cond((equal? 1 (getElementByIndex 0 2 player)) #t)
       (else #f)))

(isPlayerDone (list 'valvaro' (list 1 2 3 4 5) 0))

;;Function that makes sure everyone is ready
;;players: players of the current game
;;return: whether or not everyone has selected the stood option
(define (isEverybodyDone players)
  (cond((null? players) #t)
       ((isPlayerDone (car players)) (isEverybodyDone (cdr players)))
       (else #f)))
(isEverybodyDone (list (list 'valvaro' (list 1 2 3 4 5) 1) (list 'jon' (list 6 7 8 9 10 11) 1) (list 'crupier' (list 12 13 14 15 16 17) 0)))

;;Fucntion that changes an element in a list with another element
;;start: start of the list
;;index: index to be changed
;;element: element to be introduced
;;list1: list to be changed
;;return: the list changed
(define (changeElementByIndex start index element list1)
  (cond((null? list1) list1)
       ((> 0 index) (changeElementByIndex start (+ (listLength list1) index) element list1))
       ((= start index) (cons element (changeElementByIndex (+ start 1) index element (cdr list1))))
       (else(cons (car list1) (changeElementByIndex (+ start 1) index element (cdr list1))))))

(changeElementByIndex 0 4 8 (list 1 2 3 4 5 6 7 8 9 10))

;;Adds element to the very end of the list
;;list1: list to be processed
;;element: element to be added
;;return: the list changed
(define (addElementToLast list1 element)
  (cond((null? list1) (list element))
       (else(cons (car list1) (addElementToLast (cdr list1) element)))))

;;Updates a player's deck with a new card from the deck
;;theChosenOne: randomly generated index
;;player: player with is going to receive the card
;;deck: a player with its deck changed
(define (updateDeckForPlayer theChosenOne player deck)
  (changeElementByIndex 0 1 (addElementToLast (cadr player) (getElementByIndex 0 theChosenOne deck)) player))

(updateDeckForPlayer (random 8) (list 'valvaro' (list 1 3 5 7 9 11 13 15 17 19 21 23) 0) (list 30 40 50 60 70 80 90 100))

;;Updates the game's deck
;;theChosenOne: randomly generated index
;;deck: game's deck
;;return: a deck with it's deck changed
(define (updateMainDeck theChosenOne deck)
  (eliminateFromListByIndex deck theChosenOne 0))

(updateMainDeck (random 9) (list 1 2 3 4 5 6 7 8 9))

;;Auxilaire function for hitPlayersAux that changes the main deck
;;gameState: state of the game being processed
;;theChosenOne: randomly generated
;;return: gameStateWith its deck changed
(define (hitPlayersAux1 gameState theChosenOne)
  (changeElementByIndex 0 -1 (updateMainDeck theChosenOne (getElementByIndex 0 -1 gameState)) gameState))

;;Auxilaire function for hitPlayers that changes a players cards
;;gameState: state of the game
;;theChosenOne: randomly generated index
;;index: index of the player to be selected
;;return: gameState changed
(define (hitPlayersAux gameState theChosenOne index)
  (hitPlayers (hitPlayersAux1 (changeElementByIndex 0 index (updateDeckForPlayer theChosenOne (getElementByIndex 0 index gameState) (getElementByIndex 0 -1 gameState)) gameState) theChosenOne) (+ index 1)))

;;(hitPlayersAux (list (list 'crupier' (list 1 2 3 4) 1) (list 'p1' (list 5 6 7 8) 1) (list 'p2' (list 9 10 11 12) 1) (list 13 14 15 16 17 18 19 20)) (random  8) (random 2))

;;Case in which 1 or more players has not stood
;;gameState: state of the game
;;index: current index of player
;;return: changed state
(define (hitPlayers gameState index)
  (cond((= (- (listLength gameState) index) 1) gameState)
       ((isPlayerDone (getElementByIndex 0 index gameState)) (hitPlayers gameState (+ index 1)))
       (else(hitPlayersAux gameState (random (listLength (getElementByIndex 0 -1 gameState))) index))))

(hitPlayers (list (list 'crupier' (list 1 2 3 4) 0) (list 'p1' (list 5 6 7 8) 0) (list 'p2' (list 9 10 11 12) 0) (list 13 14 15 16 17 18 19 20)) 0)


  
  
  


  

;;(define (processTurn gameState)
  ;;(cond((isEverybodyDone gameState) (finalTurn gameState))))

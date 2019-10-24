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

;;Eliminates a element from the list based on its index
;;list1: list to be analyze
;;index: index chosen to eliminate
;;curren: index of the current process
;;return: list without the chosen element
(define (eliminateFromListByIndex list1 index currentIndex)
  (cond((null? list1) list1)
       ((> 0 index) (eliminateFromListByIndex list1 (+ (listLength list1) index) 0))
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

;;(shuffleDeck (list (list 1 'C '1)(list 2 'C '2)(list 3 'C '3)(list 4 'C '4)(list 5 'C '5)(list 6 'C '6)(list 7 'C '7)(list 8 'C '8)(list 9 'C '9)(list 10 'C '10)(list '10 'C 'J)(list 10 'C 'Q)(list '10 'C 'K)(list 1 'H '1)(list 2 'H '2)(list 3 'H '3)(list 4 'H '4)(list 5 'H '5)(list 6 'H '6)(list 7 'H '7)(list 8 'H '8)(list 9 'H '9)(list 10 'H '10)(list '10 'H 'J)(list 10 'H 'Q)(list '10 'H 'K)(list 1 'S '1)(list 2 'S '2)(list 3 'S '3)(list 4 'S '4)(list 5 'S '5)(list 6 'S '6)(list 7 'S '7)(list 8 'S '8)(list 9 'S '9)(list 10 'S '10)(list '10 'S 'J)(list 10 'S 'Q)(list '10 'S 'K)(list 1 'D '1)(list 2 'D '2)(list 3 'D '3)(list 4 'D '4)(list 5 'D '5)(list 6 'D '6)(list 7 'D '7)(list 8 'D '8)(list 9 'D '9)(list 10 'D '10)(list '10 'D 'J)(list 10 'D 'Q)(list '10 'D 'K)))

;;Function sums all the card values without counting As
;;list1: cards to be analyze
;;sum: current sum of the cards
;;return: the sum without As
(define (sumWithoutA list1 sum)
  (cond((null? list1) sum)
       ((equal? (cadar list1)  'A) (sumWithoutA (cdr list1) sum))
       (else(sumWithoutA (cdr list1) (+ (caar list1) sum)))))



;;Check whether the crupier should keep hitting the deck
;;crupierDeck: the deck the crupier ones
;;sum: current sum of the crupier's deck
;;return: whether or not the crupier should continue hitting in the next round
(define (isCrupierDone crupierDeck sum)
  (cond((null? crupierDeck) (>= sum 16))
       (else (isCrupierDone (cdr crupierDeck) (+ sum (caar crupierDeck))))))

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

;;Function that makes a player selected the "stand" option
;;player: player to be analyze
;;return: whether the player stood or not
(define (isPlayerDone player)
  (cond((equal? 1 (getElementByIndex 0 2 player)) #t)
       (else #f)))

;;Function that makes sure everyone is ready
;;players: players of the current game
;;return: whether or not everyone has selected the stood option
(define (isEverybodyDone players)
  (cond((null? players) #t)
       ((isPlayerDone (car players)) (isEverybodyDone (cdr players)))
       (else #f)))

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

;;Adds element to the very end of the list
;;list1: list to be processed
;;element: element to be added
;;return: the list changed
(define (addElementToLast list1 element)
  (cond((null? list1) (list element))
       (else(cons (car list1) (addElementToLast (cdr list1) element)))))


;;UpdatesTheSumOfTheCards
;;deck: deck to be processed
;;sum: current sum
;;return: updated Sum of cards
(define (updateSumOfCards deck sum)
  (cond((null? deck) sum)
       (else(updateSumOfCards (cdr deck) (+ sum (caar deck))))))

;;Auxilaire function for updating player's deck
;;player: player
;;return: updated deck
(define (updateDeckForPlayerAux player)
  (changeElementByIndex 0 3 (updateSumOfCards (getElementByIndex 0 1 player) 0) player))

;;Updates an A card for the crupier
;;card: card to be updated
;;return: updated card
(define (isAnA card)
  (cond((equal? 'A (caddr card)) #t)
  (else #f)))

;;Changes the value of a card
;;Value: card's new value
;;card: card to be altered
;;return: altered card
(define (giveA_CardValue value card)
  (changeElementByIndex 0 0 value card))

;;Handles an A card for the crupier
;;newCard: card to be added
;;crupier: player to be processed
;;return: updated player
(define (aCaseHandler newCard crupier)
  (cond((<= (updateSumOfCards (cadr crupier) 0) 10) (updateDeckForPlayerAux (changeElementByIndex 0 1 (addElementToLast (cadr crupier) (giveA_CardValue 11 newCard)) crupier)))
       (else(updateDeckForPlayerAux (changeElementByIndex 0 1 (addElementToLast (cadr crupier) newCard) crupier)))))

;;Updates the deck for the crupier
;;theChosenOne: randomly chosen index
;;crupier: the one to be processed
;;deck: Main game's deck
;;return: An updated crupier
(define (updateDeckForCrupier theChosenOne crupier deck)
  (cond((isAnA (getElementByIndex 0 theChosenOne deck)) (aCaseHandler (getElementByIndex 0 theChosenOne deck) crupier))
       (else(updateDeckForPlayerAux (changeElementByIndex 0 1 (addElementToLast (cadr crupier) (getElementByIndex 0 theChosenOne deck)) crupier)))))

;;Updates a player's deck with a new card from the deck
;;theChosenOne: randomly generated index
;;player: player with is going to receive the card
;;deck: a player with its deck changed
(define (updateDeckForPlayer theChosenOne player deck)
  (cond((equal? (car player) 'crupier) (updateDeckForCrupier theChosenOne player deck))
       (else(updateDeckForPlayerAux (changeElementByIndex 0 1 (addElementToLast (cadr player) (getElementByIndex 0 theChosenOne deck)) player)))))

;;Updates the game's deck
;;theChosenOne: randomly generated index
;;deck: game's deck
;;return: a deck with it's deck changed
(define (updateMainDeck theChosenOne deck)
  (eliminateFromListByIndex deck theChosenOne 0))

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

;;Changes the state of the crupier regarding whether to hit or stand
;;gameState: State of the game
;;return: changed version of the state of the game
(define (changeCrupierState gameState)
  (cond((isCrupierDone (cadar gameState) 0) (hitPlayers (changeElementByIndex 0 0 (changeElementByIndex 0 2 1 (car gameState)) gameState) 1))
       (else(hitPlayersAux gameState (random (listLength (getElementByIndex 0 -1 gameState))) 0))))

;;Case in which 1 or more players has not stood
;;gameState: state of the game
;;index: current index of player
;;return: changed state
(define (hitPlayers gameState index)
  (cond((= (- (listLength gameState) index) 1) gameState)
       ((= 0 index) (changeCrupierState gameState))
       ((isPlayerDone (getElementByIndex 0 index gameState)) (hitPlayers gameState (+ index 1)))
       (else(hitPlayersAux gameState (random (listLength (getElementByIndex 0 -1 gameState))) index))))

;;Define in the case of a sum tie whether or not the player wins
;;playerDeck: deck beloging to the player
;;crupierDeck: deck beloging to the crupier
;;return: decision
(define (unTieCase playerDeck crupierDeck)
  (cond((< (listLength playerDeck) (listLength crupierDeck)) 1)
       (else 0)))
  
;;Whether or not a player's a winner
;;crupier: the one to be compared against
;;player: player who's decision is going to be made
;;return: whether that player is a winner
(define (isWinner crupier player)
  (cond((> (getElementByIndex 0 3 player) 21) 0)
       ((> (getElementByIndex 0 3 crupier) 21) 1)
       ((< (getElementByIndex 0 3 player) (getElementByIndex 0 3 crupier)) 0)
       ((= (getElementByIndex 0 3 player) (getElementByIndex 0 3 crupier)) (unTieCase (getElementByIndex 0 1 player) (getElementByIndex 0 1 crupier)))
       (else 1)))
  

;;Case of the final turn where everybody selected the stand option
;;gameState: state of the game
;;index: current index being analyze
;;return: final decision
(define (finalTurn gameState index)
  (cond((= (- (listLength gameState) index) 1) '())
      (else (cons (isWinner (getElementByIndex 0 0 gameState) (getElementByIndex 0 index gameState)) (finalTurn gameState (+ index 1))))))

;;processing for the turn
;;gameState: state of the game
;;return: next state of the game
(define (processTurn gameState)
  (cond((isEverybodyDone (eliminateFromListByIndex gameState -1 0)) (finalTurn gameState 1))
       (else(hitPlayers gameState 0))))

(processTurn (list (list 'crupier '() 0 0) (list 'p1 '() 0 0) (list 'p2 '() 0 0) (shuffleDeck (list(list 1 'C 'A)(list 2 'C '2)(list 3 'C '3)(list 4 'C '4)(list 5 'C '5)(list 6 'C '6)(list 7 'C '7)(list 8 'C '8)(list 9 'C '9)(list 10 'C '10)(list '10 'C 'J)(list 10 'C 'Q)(list '10 'C 'K)(list 1 'H 'A)(list 2 'H '2)(list 3 'H '3)(list 4 'H '4)(list 5 'H '5)(list 6 'H '6)(list 7 'H '7)(list 8 'H '8)(list 9 'H '9)(list 10 'H '10)(list '10 'H 'J)(list 10 'H 'Q)(list '10 'H 'K)(list 1 'S 'A)(list 2 'S '2)(list 3 'S '3)(list 4 'S '4)(list 5 'S '5)(list 6 'S '6)(list 7 'S '7)(list 8 'S '8)(list 9 'S '9)(list 10 'S '10)(list '10 'S 'J)(list 10 'S 'Q)(list '10 'S 'K)(list 1 'D 'A)(list 2 'D '2)(list 3 'D '3)(list 4 'D '4)(list 5 'D '5)(list 6 'D '6)(list 7 'D '7)(list 8 'D '8)(list 9 'D '9)(list 10 'D '10)(list '10 'D 'J)(list 10 'D 'Q)(list '10 'D 'K)))))
(processTurn (list (list 'crupier (list (list 11 'S 'A) (list 5 'D '5)) 1 16) (list 'p1 (list (list 8 'S '8) (list 5 'D '5)) 1 13) (list 'p2 (list (list 3 'S '3) (list 5 'D '5)) 1 8) '()))
#lang racket

(require racket/gui)

(define lista '(a b c))

; Cambia la variable lista en base a otra lista que pasamos
(define (change l)
  (set! lista l))

; Converts a list to a string
; lst: Is the list we take to convert into a string
(define (list->string lst)
  (string-join (map symbol->string lst) " "))
1
; Modifica una variable
(define-syntax-rule (setter variable condition value)
  (if condition (begin (set! variable value) variable) variable))

; Anade un mensage a un contenedor, txt: es el mensage / parent: es el contenedor donde se va a anadir
(define (addMsg txt parent)
  (new message% [parent parent][label txt]))

; Sets the turn message of all the players to their winning status
; players: a list of boolean representing if the palyer won or lost
(define (winners players)
  ((cond ((equal? (car players) 1)
          (send turn_A set-label "Won!"))
         (else
          (send turn_A set-label "Lost")))
   (cond ((equal? (cadr players) 1)
          (send turn_B set-label "Won!"))
         (else
          (send turn_B set-label "Lost")))
   (cond ((equal? (caddr players) 1)
          (send turn_C set-label "Won!"))
         (else
          (send turn_C set-label "Lost")))))

; G U I 

; Make a new frame where everything will be placed
(define blackJack (new frame%
                       [label "BlaCEkJack"]
                       [x 550]
                       [y 200]))

; Create a panel for the House
(define panel_H (new vertical-panel%
                     [parent blackJack]
                     [alignment '(center top)]
                     [min-height 140]
                     [horiz-margin 15]))

(define name_H (new message% [parent panel_H][label "Crupier"]))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

; Create a player panel to place al the players
(define players_Panel (new horizontal-panel%
                           [parent blackJack]
                           [alignment '(center bottom)]
                           [min-height 185]
                           [horiz-margin 15]))

; A

; Create a panel for player A inside the player panel
(define panel_A (new vertical-panel% [parent players_Panel][alignment '(center bottom)]))

; Creates messages for player A info
(define name_A (new message% [parent panel_A][label ""][min-width 50]))
(define turn_A (new message% [parent panel_A][label "Plays"][min-width 35]))

; Creates a panel for the buttons of player A
(define buttons_A (new horizontal-panel% [parent panel_A][alignment '(center top)]))

; Create two buttons
(new button% [parent buttons_A]
             [label "Stand"]
             [callback (lambda (button event)
                         ((send turn_A set-label "Waits")
                          (send turn_B set-label "Plays")))])

(new button% [parent buttons_A]
             [label "Play"]
             [callback (lambda (button event)
                         ((send turn_A set-label "Waits")
                          (send turn_B set-label "Plays")))])

; Dialog for choosing
(define dialog_A (instantiate dialog% ("YOU GOT AN ACE!"))) 
(new message% [parent dialog_A] [label "Would you like it to be 1 or 11?"][min-width 175][horiz-margin 5])
(define dbp_A (new horizontal-panel% [parent dialog_A][alignment '(center center)]))
(new button% [parent dbp_A] [label "11"] [callback (lambda (button event)(addMsg "J" panel_A))])
(new button% [parent dbp_A] [label "1"] [callback (lambda (button event)(addMsg "A" panel_A))])

; B

; Create a panel for player B inside the player panel
(define panel_B (new vertical-panel% [parent players_Panel][alignment '(center bottom)]))

; Creates messages for player B info
(define name_B (new message% [parent panel_B][label ""][min-width 50]))
(define turn_B (new message% [parent panel_B][label "Waits"][min-width 35]))

; Creates a panel for the buttons of player B
(define buttons_B (new horizontal-panel% [parent panel_B][alignment '(center top)]))

; Create two buttons
(new button% [parent buttons_B]
             [label "Stand"]            
             [callback (lambda (button event)
                         ((send turn_B set-label "Waits")
                          (send turn_C set-label "Plays")))])

(new button% [parent buttons_B]
             [label "Play"]
             [callback (lambda (button event)
                         (send turn_B set-label "Waits")
                         (send turn_C set-label "Plays"))])

; Dialog for choosing
(define dialog_B (instantiate dialog% ("YOU GOT AN ACE!"))) 
(new message%[parent dialog_B][label "Would you like it to be 1 or 11?"][min-width 175][horiz-margin 5])
(define dbp_B (new horizontal-panel%[parent dialog_B][alignment '(center center)]))
(new button% [parent dbp_B] [label "11"] [callback (lambda (button event)(addMsg "J" panel_B))])
(new button% [parent dbp_B] [label "1"] [callback (lambda (button event)(addMsg "A" panel_B))])

; C

; Create a panel for player C inside the player panel
(define panel_C (new vertical-panel% [parent players_Panel][alignment '(center bottom)]))

; Creates messages for player C info
(define name_C (new message% [parent panel_C][label "Waits"][min-width 50]))
(define turn_C (new message% [parent panel_C][label "Waits"][min-width 35]))

; Creates a panel for the buttons of player C
(define buttons_C (new horizontal-panel% [parent panel_C][alignment '(center top)]))

; Create two buttons
(new button% [parent buttons_C]
             [label "Stand"]
             [callback (lambda (button event)
                         ((send turn_C set-label "Waits")
                          (send turn_A set-label "Plays")))])

(new button% [parent buttons_C]
             [label "Play"]
             [callback (lambda (button event)
                         ((send turn_C set-label "Waits")
                          (send turn_A set-label "Plays")))])

; Dialog for choosing
(define dialog_C (instantiate dialog% ("YOU GOT AN ACE!"))) 
(new message% [parent dialog_C] [label "Would you like it to be 1 or 11?"][min-width 175][horiz-margin 5])
(define dbp_C (new horizontal-panel% [parent dialog_C][alignment '(center center)]))
(new button% [parent dbp_C] [label "11"] [callback (lambda (button event)(addMsg "J" panel_C))])
(new button% [parent dbp_C] [label "1"] [callback (lambda (button event)(addMsg "A" panel_C))])

(define (blaCEkJack a b c)
  ((send name_A set-label a)
   (send name_B set-label b)
   (send name_C set-label c)
   (send blackJack show #t)))


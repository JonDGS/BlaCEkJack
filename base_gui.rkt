#lang racket

(require racket/gui)

; Make a frame by instantiating the frame% class
(define blackJack (new frame% [label "BlackJack"]))

(define casa (new vertical-panel% [parent blackJack]
                                     [alignment '(center top)]))

(define msg (new message% [parent casa]
                          [label "Turno"]))

(define casa_b (new horizontal-panel% [parent casa]
                                     [alignment '(center top)]))

(new button% [parent casa_b]
             [label "Stand"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Se paro de jugar"))])

(new button% [parent casa_b]
             [label "Play"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Se jugo"))])

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; 

(define jugadores (new horizontal-panel% [parent blackJack]
                                     [alignment '(center bottom)]))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

(define jugador_A (new vertical-panel% [parent jugadores]
                                     [alignment '(center bottom)]))

(define msg_A (new message% [parent jugador_A]
                          [label "Turno"]))

(define jA_b (new horizontal-panel% [parent jugador_A]
                                     [alignment '(center top)]))
(new button% [parent jA_b]
             [label "Stand"]
             [callback (lambda (button event)
                         (send msg_A set-label "Se paro de jugar"))])

(new button% [parent jA_b]
             [label "Play"]
             [callback (lambda (button event)
                         (send msg_A set-label "Se jugo"))])

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

(define jugador_B (new vertical-panel% [parent jugadores]
                                     [alignment '(center bottom)]))

(define msg_B (new message% [parent jugador_B]
                          [label "Turno"]))

(define jB_b (new horizontal-panel% [parent jugador_B]
                                     [alignment '(center top)]))
(new button% [parent jB_b]
             [label "Stand"]
             [callback (lambda (button event)
                         (send msg_B set-label "Se paro de jugar"))])

(new button% [parent jB_b]
             [label "Play"]
             [callback (lambda (button event)
                         (send msg_B set-label "Se jugo"))])

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

(define jugador_C (new vertical-panel% [parent jugadores]
                                     [alignment '(center bottom)]))

(define msg_C (new message% [parent jugador_C]
                          [label "Turno"]))

(define jC_b (new horizontal-panel% [parent jugador_C]
                                     [alignment '(center top)]))
(new button% [parent jC_b]
             [label "Stand"]
             [callback (lambda (button event)
                         (send msg_C set-label "Se paro de jugar"))])

(new button% [parent jC_b]
             [label "Play"]
             [callback (lambda (button event)
                         (send msg_C set-label "Se jugo"))])

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

; Show the frame by calling its show method
(send blackJack show #t)



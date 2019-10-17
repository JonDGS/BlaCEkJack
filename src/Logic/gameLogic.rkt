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

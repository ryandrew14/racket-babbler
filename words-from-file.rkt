#lang racket
(require 2htdp/batch-io)

(provide
 
 (contract-out
  (get-words (-> string? [listof string?]))))
  

(define example-1strlist
  (list "a" "t" " " "t" "h" "e" "\n" " " "\n" " " "h" "o" "u" "s" "e" " " "\n"))

;; 1String -> Boolean
;; is the 1string a delimiter
(define (delimiter? 1str)
  (or (string=? " " 1str) (string=? "\n" 1str)))

;; File -> [Listof String]
;; reads the file as a list of 1strings, then concats the strings that aren't a space or a newline
;; into words
(define (get-words f)
  (define 1strs (read-1strings f))
  (define words (make-words 1strs))
  words)

;; [Listof 1String] -> [Listof String]
;; cuts out all "\n" and " " characters from the given list, concatenating the rest of the characters
;; into words as they are separated by the removed characters
(define (make-words l)
  (define l-working (remove-delimiters l))
  (cond
    [(empty? l-working) '()]
    [(cons (make-first-word l-working)
           (make-words (remove-first-word l-working)))]))

;; [Listof 1String] -> String
;; makes the first word not separated by a " " or "\n" character from the given list of 1strings
(define (make-first-word l)
  (cond
    [(empty? l) ""]
    [(delimiter? (car l)) (make-first-word (cdr l))]
    [else (begin-make-first-word l)]))

;; [Listof 1String] -> String
;; makes the first word as stated above, except that this list now has all the " " and "\n" filtered
;; out of it
(define (begin-make-first-word l)
  (cond
    [(empty? l) ""]
    [(delimiter? (car l)) ""]
    [else (string-append (car l) (begin-make-first-word (cdr l)))]))

;; [Listof 1String] -> [Listof 1String]
;; removes the first word not separated by a " " or "\n" character
;; IDEA goes along the string until it gets to one of the specified characters
;; then passes off to a different function
(define (remove-first-word l)
  (cond
    [(empty? l) '()]
    [(delimiter? (first l)) (remove-delimiters l)]
    [else (remove-first-word (cdr l))]))

;; [Listof 1String] -> [Listof 1String]
;; removes the delimiters until the next word
(define (remove-delimiters l)
  (cond
    [(empty? l) '()]
    [(delimiter? (first l)) (remove-delimiters (rest l))]
    [else l]))




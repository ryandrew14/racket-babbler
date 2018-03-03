#lang racket

(require "words-from-file.rkt"
         "wordlist-processing.rkt"
         math/matrix)

(define test2 '("a" "b" "c" "a" "b" "d"))
(define ph1 (ph (list "a" "b") (list "c" "d")))
(define ph2 (ph (list "b" "c") (list "a")))
(define ph3 (ph (list "c" "a") (list "b")))
(define ph4 (ph (list "a" "b") (list "d")))
(define ph5 (ph (list "a" "b") (list "c")))
(define phtest1 (list ph5 ph2 ph3 ph4))
(define phtest (list ph2 ph3 ph1))

(define (start/file file N)
  (define words (get-words file))
  (define states (get-states words))
  (start states N))

;; [Listof ProbHolder] N -> String
;; starts Markov babbler
(define (start states N)
  (define rand (random (length states)))
  (define current (list-ref states rand))
  (main current states N))

;; ProbHolder [Listof ProbHolder] N -> String
;; Finite state machine where the states are ProbHolders
;; generates an n word statement via a markov chain
(define (main current states n)
  (cond
    [(zero? n) ""]
    [(empty? (ph-values current)) ""]
    [else (define next-state (next-ph current states))
          (string-append (cadr (ph-pair next-state)) " " (main next-state states (- n 1)))]))

;; ProbHolder [Listof ProbHolder] -> ProbHolder
;; gets the next state (at random) from the list of states and returns it
(define (next-ph current states)
  (define rand (random (length (ph-values current))))
  (define next-word (list-ref (ph-values current) rand))
  (define next-pair (list (cadr (ph-pair current)) next-word))
  (define next-state (find-pair next-pair states))
  next-state)

;; [Pair String String] [Listof ProbHolder] -> ProbHolder
;; matches the given pair with the pair of a ProbHolder in states
(define (find-pair pair states)
  (cond
    [(empty? states) (ph pair '())]
    [(pair=? pair (ph-pair (car states))) (car states)]
    [else (find-pair pair (rest states))]))
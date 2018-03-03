#lang racket

(require "words-from-file.rkt" ;; provides get-words, which gets a list of all the words in a txt file
         math/matrix)

(module+ test
  (require rackunit))

(provide
 ph
 ph-pair
 ph-values
 get-states
 ph->list
 pair=?)

; A Probablility-Holder (ProbHolder) is a
; - (ProbHolder [Pair String String] [Listof [Pair String Number]])
; where pair is the pair that the markov chain is looking for and the values are the list of words
; that follow that combo, along with how many of them there are
(struct ph [pair values])

(define test (get-words "test.txt"))
(define test2 '("a" "b" "c" "a" "b" "d"))
(define ph1 (ph (list "a" "b") (list "c" "d")))
(define ph2 (ph (list "b" "c") (list "a")))
(define ph3 (ph (list "c" "a") (list "b")))
(define ph4 (ph (list "a" "b") (list "d")))
(define ph5 (ph (list "a" "b") (list "c")))
(define phtest1 (list ph5 ph2 ph3 ph4))
(define phtest (list ph2 ph3 ph1))

;; [Listof String] -> [Listof ProbHolder]
;; condenses a list of strings into ProbHolders for use in a Markov matrix
(define (get-states los)
  (condense-phs (make-phs (make-triples los))))

;; [Listof String] -> [Listof [Tuple String String String]]
;; create creates lists of successive words from the given list of strings
;; has a helper because it will throw an error if the user tries to initialize the process
;; with a file that's too small
(define (make-triples los)
  (cond
    [(> 3 (length los)) (error "Cannot use markov babbler - file too small!")]
    [else (mst-rec los)]))

;; [Listof String] -> [Listof [Tuple String String String]]
;; create creates lists of successive words from the given list of strings
(define (mst-rec los)
  (cond
    [(> 3 (length los)) '()]
    [else (cons (list (car los) (cadr los) (caddr los)) (mst-rec (cdr los)))]))

;; [Listof [Tuple String String String]] -> [Listof ProbHolder]
;; makes all the triples in this list into ProbHolders
(define (make-phs ts)
  (map (λ (tuple) (ph (list (car tuple) (cadr tuple)) (list (caddr tuple)))) ts))

;; [Listof ProbHolder] -> [Listof ProbHolder]
;; condenses ProbHolders with the same Pair, making one ProbHolder with multiple values
(define (condense-phs phs)
  (cond
    [(empty? phs) '()]
    [(has-duplicate? (car phs) (cdr phs)) (condense-phs (combine (car phs) (cdr phs)))]
    [else (cond (first phs) (condense-phs (rest phs)))]))

;; ProbHolder [Listof ProbHolder] -> [Listof ProbHolder]
;; combines phr into its duplicate in phs
;; this function can only be called when there is definitely a duplicate for phr in phs
(define (combine phr phs)
  (cond
    [(not (has-duplicate? phr phs))
     (error "(combine ...) can only be called if given pair has a duplicate in given list!")]
    [(empty? phs) (list phr)]
    [else (if (pair=? (ph-pair (car phs)) (ph-pair phr))
              (cons (condense-dup phr (car phs)) (cdr phs))
              (cons (car phs) (combine phr (cdr phs))))]))

;; ProbHolder ProbHolder -> ProbHolder
;; condenses the two ProbHolders, appending their values and using the pair of the first one
;; intended to be used to combine duplicates
(define (condense-dup p1 p2)
  (ph (ph-pair p1) (append (ph-values p1) (ph-values p2))))

;; ProbHolder [Listof ProbHolder] -> Boolean
;; does the given ProbHolder have the same pair as any of the ProbHolders in phs
(define (has-duplicate? ph phs)
  (ormap (λ (given) (pair=? (ph-pair ph) (ph-pair given))) phs))

;; Pair Pair -> Boolean
;; are these the same pair
(define (pair=? p1 p2)
  (and (string=? (car p1) (car p2)) (string=? (cadr p1) (cadr p2))))

;; ProbHolder -> [Pair [Pair String String] [Listof String]]
;; turns a ProbHolder into two lists
(define (ph->list ph)
  `(,(ph-pair ph) ,(ph-values ph)))

(module+ test
  (check-equal? (ph-values (first (condense-phs phtest1)))
                (ph-values (first phtest)))
  (check-equal? (map (λ (x) (ph->list x)) (condense-phs phtest1))
              (map (λ (x) (ph->list x)) phtest)))
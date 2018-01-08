#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "lab1.rkt")

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers
  (integers-from 0))

(define hw2-tests
  (test-suite
   "Homework 2 Tests"

   (test-case
    "stream-scan"
    (check-equal? (stream->list (stream-scan + 0 empty-stream))
                  '(0)
                  "stream-scan base case")
    (check-equal? (stream->list (stream-scan + 0 (stream 1 2 3 4 5 6)))
                  '(0 1 3 6 10 15 21)
                  "stream-scan inductive case"))

   (test-case
    "stream-take-n"
    (check-equal? (stream-take-n 10 integers)
                  '(0 1 2 3 4 5 6 7 8 9)
                  "(stream-take-n 10 integers)"))

   (test-case
    "stream-pair-with"
    (check-equal? (stream->list (stream-pair-with (lambda (x) (+ x 1)) (stream 1 2 3 4)))
                  '((1 . 2) (2 . 3) (3 . 4) (4 . 5))
                  "(stream-pair-with (lambda (x) (+ x 1)) (1 2 3 4))"))

   (test-case
    "cycles-list"
    (check-equal? (stream-take-n 8 (cycle-lists '(1 2 3) '("a" "b")))
                  '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a") (2 . "b"))
                  "(stream-take-n 8 (cycle-lists '(1 2 3) '(\"a\" \"b\")))"))

   (test-case
    "seen"
    (check-equal? (seen 5)
                  #f
                  "(seen 5)")
    (check-equal? (begin (seen 5) (seen 5))
                  #t
                  "(begin (seen 5) (seen 5))")
    (check-equal? (begin (seen 5) (seen 10))
                  #f
                  "(begin (seen 5) (seen 10))"))
))

(when (not (eq? (run-tests hw2-tests) 0))
    (exit 1))

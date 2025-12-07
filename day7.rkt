#lang racket
(define input (file->lines "f:/Niklas/src/aoc2025/input/day7.txt"))

(define example (list
  ".......S......." "..............."
  ".......^......." "..............."
  "......^.^......" "..............."
  ".....^.^.^....." "..............."
  "....^.^...^...." "..............."
  "...^.^...^.^..." "..............."
  "..^...^.....^.." "..............."
  ".^.^.^.^.^...^." "..............."))

(define (parse input)
  (define start (string-find (car input) "S"))
  (define rows (for/list ((y (in-range (length input)))
                          (s input))
                 (define splits (for/set ((j (in-range (string-length s)))
                                          (c (string->list s))
                                          #:when (char=? c #\^))
                                  j))
                 (list y splits)))
  (list start rows))

(define (process row rays)
  (define (handle ray out)
    (if (set-member? row ray)
        (list (+ 1 (car out)) (set-union (cadr out) (set (- ray 1) (+ ray 1))))
        (list (car out) (set-add (cadr out) ray))))
  (foldl handle (list (car rays) (set)) (set->list (cadr rays))))

(define (paths row rays)
  (define (handle ray out)
    (let ((last (car ray))
          (add-last (lambda (v) (+ v (cdr ray)))))
      (if (set-member? row last)
          (hash-update (hash-update out (+ last 1) add-last 0) (- last 1) add-last 0)
          (hash-update out last add-last 0))))
  (foldl handle (hash) (hash->list rays)))

(define (solve input)
  (define parsed (parse input))
  (define start (set (car parsed)))
  (define rows (map cadr (cadr parsed)))
  (car (foldl process (list 0 start) rows)))

(define (solve2 input)
  (define parsed (parse input))
  (define start (hash (car parsed) 1))
  (define rows (map cadr (cadr parsed)))
  (define final (foldl paths start rows))
  (foldl + 0 (hash-values final)))

(solve example)
(solve input)
(solve2 example)
(solve2 input)
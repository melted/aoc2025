#lang racket

(define example (vector "..@@.@@@@." "@@@.@.@.@@"
                        "@@@@@.@.@@" "@.@@@@..@."
                        "@@.@@@@.@@" ".@@@@@@@.@"
                        ".@.@.@.@@@" "@.@@@.@@@@"
                        ".@@@@@@@@." "@.@.@@@.@."))

(define input (list->vector (file->lines "F:/Niklas/src/aoc2025/input/day4.txt")))

(define (at m x y)
  (if (and (<= 0 x (- (width m) 1)) (<= 0 y (- (height m) 1)))
      (string-ref (vector-ref m y) x)
      #\.))

(define (set-at m x y c)
  (string-set! (vector-ref m y) x c))

(define (width m)
  (string-length (vector-ref m 0)))

(define (height m)
  (vector-length m))

(define (neighbors m x y)
  (for*/list ((i '(-1 0 1))
             (j '(-1 0 1))
             #:unless (= i j 0))
    (at m (+ x i) (+ y j))))

(define (count-rolls m x y)
  (define nb (neighbors m x y))
  (length (filter (λ (c) (char=? c #\@)) nb)))

(define (removable input)
  (for*/list ((y (in-range 0 (height input)))
             (x (in-range 0 (width input)))
             #:when (char=? (at input x y) #\@)
             #:when  (< (count-rolls input x y) 4))
    (vector x y)))

(define (solve input)
  (length (removable input)))

(define (iterate input)
  (define to-remove (removable input))
  (define new-map (vector-map string-copy input))
  (for ((p to-remove))
    (set-at new-map (vector-ref p 0) (vector-ref p 1) #\.))
  (values (length to-remove) new-map))

(define (solve2 input)
  (define (worker m acc)
    (define-values (n next) (iterate m))
    (if (= n 0)
        acc
        (worker next (+ acc n))))
  (worker input 0))

(solve example)
(solve input)
(solve2 example)
(solve2 input)
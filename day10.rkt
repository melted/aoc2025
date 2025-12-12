#lang racket

(define input (file->lines "F:/Niklas/src/aoc2025/input/day10.txt"))

(define example (list
                 "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                 "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                 "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"))

(struct machine (lights buttons joltage) #:transparent)

(define (parse-machine str)
  (define line #px"\\[(.*)\\]|\\(([0-9,]*)\\)|\\{(.*)\\}")
  (define matches (regexp-match* line str #:match-select values))
  (define (parse-nums str)
    (map string->number (string-split str ",")))
  (define (parse-lights x)
    (foldl (λ (b acc)
             (+ (if (char=? b #\#) 1 0)
                (* 2 acc)))
           0
           (reverse x)))
  (define (parse-button nums)
    (foldl (λ (n acc) (+ (expt 2 n) acc)) 0 nums))
  (define lights
    (parse-lights (string->list (car (filter-map cadr matches)))))
  (define buttons  (map parse-button
                        (map parse-nums
                             (filter-map caddr matches))))
  (define joltages (list->vector (parse-nums (car (filter-map cadddr matches)))))
  (machine lights buttons joltages))

(define (select ls n)
  (if (null? ls)
      '()
      (let ((h (car ls))
            (t (cdr ls)))
        (if (= n 1)
          (cons (list h) (select t 1))
          (append (map (λ (l) (cons h l)) (select t (- n 1)))
                (select t n))))))

(define (solve input)
  (define machines (map parse-machine input))
  (define (press? m bts)
    (= (machine-lights m) (apply bitwise-xor bts)))
  (define (min-press m)
    (for*/first ((i (in-range 1 (length (machine-buttons m))))
                 (bts (select (machine-buttons m) i))
                 #:when (press? m bts))
      (printf "~a: ~a\n" m i)
      i))
  (for/sum ((m machines))
    (min-press m)))
#lang racket

(define example "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(define input (string-trim (file->string "F:/Niklas/src/aoc2025/input/day2.txt") "\n"))

(struct id-range (start end) #:transparent)

(define (parse input)
  (define ranges (string-split input ","))
  (define (parse-range str)
    (match (string-split str "-")
      ((list start end) (id-range (string->number start)
                               (string->number end)))))
  (map parse-range ranges))

(parse example)

(define (digits n)
  (let ((next (quotient (abs n) 10)))
    (if (= next 0) 1 (+ (digits next) 1))))

(define (is-doublet? n)
  (define d (digits n))
  (define h (quotient d 2))
  (if (even? d)
      (let ((s (expt 10 h)))
        (= (quotient n s) (remainder n s)))
      #f))

(define (next-doublet n)
  (define d (digits n))
  (define h (quotient d 2))
  (if (odd? d)
      (+ (expt 10 d) (expt 10 h))
      (let* ((s (expt 10 h))
             (front (quotient n s))
             (back (remainder n s)))
        (if (< back front)
            (+ (* front s) front)
            (let ((t (+ front 1)))
              (if (= s t)
                  (next-doublet (expt 10 d))
                  (+ (* (+ front 1) s) (+ front 1))))))))

(define (score-range r)
  (define (worker n acc)
    (if (> n (id-range-end r))
        acc
        (worker (next-doublet n) (+ acc (if (is-doublet? n) n 0)))))
  (worker (id-range-start r) 0))

(define (solve input)
  (foldl + 0 (map score-range (parse input))))

(solve example)
(solve input)

(define (generate-repeated m n)
  (define t (expt 10 n))
  (define b (expt 10 (- n 1)))
  (for/set ((i (in-range b t)))
    (let loop ((v 0) (p 0))
      (if (= m p)
          v
          (loop (+ v (* i (expt t p))) (+ p 1))))))

(define (all-repeated)
  (define singles (map (λ (n) (generate-repeated n 1)) (range 2 10)))
  (apply set-union (append singles (list (generate-repeated 2 2)
                                         (generate-repeated 3 2)
                                         (generate-repeated 4 2)
                                         (generate-repeated 5 2)
                                         (generate-repeated 2 3)
                                         (generate-repeated 3 3)
                                         (generate-repeated 2 4)
                                         (generate-repeated 2 5)))))

(define (doubles)
  (set-union (generate-repeated 2 1)
             (generate-repeated 2 2)
             (generate-repeated 2 3)
             (generate-repeated 2 4)
             (generate-repeated 2 5)))


(define (solve2 input)
  (define ranges (parse input))
  (define all-vals (all-repeated))
  (define (check-range r)
    (let loop ((n (id-range-start r)) (acc 0))
      (if (> n (id-range-end r))
          acc
          (loop (+ n 1) (+ acc (if (set-member? all-vals n) n 0))))))
  (foldl + 0 (map check-range ranges)))

(solve2 example)
(solve2 input)

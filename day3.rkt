#lang racket

(define input (file->lines "F:/Niklas/src/aoc2025/input/day3.txt"))

(define example (list "987654321111111"
                      "811111111111119"
                      "234234234234278"
                      "818181911112111"))

(define (parse str)
  (list->vector (map (λ (n) (- (char->integer n) 48)) (string->list str))))

(define (find-result v n)
  (define len (vector-length v))
  (let loop ((i 0) (m 0) (mi 0) (r n) (acc 0))
      (if (= r 0)
          acc
          (if (= i (- len r -1))
              (loop (+ mi 1) 0 0 (- r 1) (+ (* 10 acc) m))
              (let ((z (vector-ref v i)))
                (if (> z m)
                    (loop (+ i 1) z i r acc)
                    (loop (+ i 1) m mi r acc)))))))
  
(define (solve n input)
  (foldl + 0 (map (λ (v) (find-result v n)) (map parse input))))

(solve 2 example)
(solve 2 input)
(solve 12 example)
(solve 12 input)

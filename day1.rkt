#lang racket

(define input (file->lines "F:/Niklas/src/aoc2025/input/day1.txt"))

(define (parse entry)
  (define dir
    (match (string-ref entry 0)
      [#\L -1]
      [#\R 1]))
  (define num (string->number (substring entry 1)))
  (* dir num))

(define (solve input)
  (for/fold ([current 50]
             [zeros 0]
             #:result zeros)
            ([x (map parse input)])
    (let ([next (modulo (+ current x) 100)])
      (values next
              (if (= next 0)
                  (+ zeros 1)
                  zeros)))))

(define example (list "L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"))

(solve example)
(solve input)

(define (clicks pos rot)
    (when (and (= pos 0) (< rot 0))
        (set! pos 100)) ; ugly edge case, what are you gonna do?
    (quotient (+ (abs (+ pos rot -50)) 50) 100))

(define (solve2 input)
  (for/fold ([current 50]
             [zeros 0]
             #:result zeros)
            ([x (map parse input)])
    (let ([next (modulo (+ current x) 100)])
      (values next
              (+ zeros (clicks current x))))))

(solve2 example)
(solve2 input)

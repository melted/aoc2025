#lang racket
(define input (file->lines "f:/Niklas/src/aoc2025/input/day6.txt"))

(define example (list "123 328  51 64" " 45 64  387 23"
                      "  6 98  215 314" "*   +   *   +"))

(struct column (op nums) #:transparent)

(define (parse input)
  (define-values (ops nums) (split-at (reverse input) 1))
  (define opsv (list->vector (string-split (car ops))))
  (define numv (map (λ (n)
                      (list->vector
                       (map string->number
                            (string-split n))))
                    nums))
  (for/list ((i (in-range (vector-length opsv))))
    (column (vector-ref opsv i)
            (map (λ (v) (vector-ref v i)) numv))))

(define (calculate c)
  (define op (match (column-op c)
               ("*" *)
               ("+" +)))
  (apply op (column-nums c)))

(define (solve input)
  (for/sum ((c (parse input)))
    (calculate c)))

(define (non-empty? s)
  (non-empty-string? (string-trim s)))

(define (parse2 input)
  (define cols
    (for/list ((i (in-range (string-length (car input)))))
      (list->string (map (λ (s) (if (< i (string-length s))
                                    (string-ref s i)
                                    #\space))
                         input))))
  (define (parse-chunk c)
    (define op (string-ref (car c)
                           (- (string-length (car c)) 1)))
    (define (only-numbers s)
      (list->string (filter char-numeric? (string->list s))))
    (column (string op) (map (λ (s)
                               (string->number (only-numbers s)))
                             c)))
  (let loop ((rest cols) (out '()))
    (if (null? rest)
        out
        (let-values (((next tail) (splitf-at rest non-empty?)))
          (let ((new-rest (if (null? tail) tail (drop tail 1))))
            (loop new-rest (cons (parse-chunk next) out)))))))

(define (solve2 input)
  (for/sum ((c (parse2 input)))
    (calculate c)))

(solve example)
(solve input)
(solve2 example)
(solve2 input)
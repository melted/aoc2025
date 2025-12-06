#lang racket

(struct id-range (start end) #:transparent)

(define input (file->lines "./input/day5.txt"))

(define example (list "3-5" "10-14" "16-20" "12-18" "" "1" "5" "8" "11" "17" "32"))

(define (parse input)
  (define-values (filters vals)
    (splitf-at input (lambda (s) (> (string-length s) 0))))
  (define (parse-range s)
    (match (string-split s "-")
      ((list start end)
       (id-range (string->number start)
                 (string->number end)))))
  (vector (map parse-range filters)
          (map string->number (drop vals 1))))

(define (in-id-range r n)
  (<= (id-range-start r) n (id-range-end r)))

(define (solve input)
  (define (fresh filters)
    (lambda (n)
      (ormap (lambda (r) (in-id-range r n)) filters)))
  (match (parse input)
    ((vector filters vals)
     (length (filter (fresh filters) vals)))))

(define (merge-overlapping filters)
  (define sorted (sort filters
                       (lambda (a b) (< (id-range-start a)
                                        (id-range-start b)))))
  (let loop ((elem (car sorted)) (rest (cdr sorted)) (out '()))
    (if (null? rest)
        (cons elem out)
        (let ((next (car rest)))
          (if (<= (id-range-start next) (id-range-end elem))
              (loop (id-range (id-range-start elem)
                              (max (id-range-end elem)
                                   (id-range-end next)))
                    (cdr rest)
                    out)
              (loop next (cdr rest) (cons elem out)))))))

(define (solve2 input)
  (define filters (vector-ref (parse input) 0))
  (define separate (merge-overlapping filters))
  (for/sum ((f separate))
    (+ 1 (- (id-range-end f) (id-range-start f)))))

(solve example)
(solve input)
(solve2 example)
(solve2 input)

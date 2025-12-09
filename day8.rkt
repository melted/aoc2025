#lang racket

(define input (file->lines "f:/Niklas/src/aoc2025/input/day8.txt"))

(define example (list "162,817,812" "57,618,57" "906,360,560"
                      "592,479,940" "352,342,300" "466,668,158"
                      "542,29,236" "431,825,988" "739,650,466"
                      "52,470,668" "216,146,977" "819,987,18"
                      "117,168,530" "805,96,715" "346,949,466"
                      "970,615,88" "941,993,340" "862,61,35"
                      "984,92,344" "425,690,689"))

(struct pos (x y z) #:transparent)

(define (parse-entry str)
  (define x (map string->number (string-split str ",")))
  (match x
    ((list x y z) (pos x y z))))

(define (parse input)
  (list->vector (map parse-entry input)))

(define (sqr n) (* n n))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (pos-x p1) (pos-x p2)))
           (sqr (- (pos-y p1) (pos-y p2)))
           (sqr (- (pos-z p1) (pos-z p2))))))

(define (distances points)
  (define dists
    (for*/vector ((i (in-range (vector-length points)))
              (j (in-range (+ i 1) (vector-length points))))
      (let ((dist (distance (vector-ref points i) (vector-ref points j))))
        (vector dist i j))))
  (vector-sort dists (λ (a b) (< (vector-ref a 0) (vector-ref b 0)))))

(define (solve input)
  (define points (parse input))
  (define size (if (< (length input) 1000) 10 1000))
  (define dists (take (vector->list (distances points)) size))
  (define members (list->vector (range (vector-length points))))
  (define clusters
    (let ((h (make-hash)))
      (for ((i (in-range (vector-length points))))
        (hash-set! h i (set i)))
      h))
  (for ((d dists))
    (let ((a (vector-ref d 1))
          (b (vector-ref d 2)))
      (let ((ma (vector-ref members a))
            (mb (vector-ref members b)))
        (unless (= ma mb)
          (for ((p (hash-ref clusters mb)))
            (vector-set! members p ma))
          (hash-update! clusters ma (λ (v) (set-union v (hash-ref clusters mb))))
          (hash-remove! clusters mb)))))
  (let ((csizes (hash-map clusters (λ (k v) (set-count v)))))
    (foldl * 1 (take (sort csizes >) 3))))

(define (solve2 input)
  (define points (parse input))
  (define dists (vector->list (distances points)))
  (define members (list->vector (range (vector-length points))))
  (define clusters
    (let ((h (make-hash)))
      (for ((i (in-range (vector-length points))))
        (hash-set! h i (set i)))
      h))
  (for/last ((d dists)
        #:break (= 1 (hash-count clusters)))
    (let ((a (vector-ref d 1))
          (b (vector-ref d 2)))
      (let ((ma (vector-ref members a))
            (mb (vector-ref members b)))
        (unless (= ma mb)
          (for ((p (hash-ref clusters mb)))
            (vector-set! members p ma))
          (hash-update! clusters ma (λ (v) (set-union v (hash-ref clusters mb))))
          (hash-remove! clusters mb)))
      (* (pos-x (vector-ref points a)) (pos-x (vector-ref points b))))))
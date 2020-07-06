
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1 - sequence
(define (sequence low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))

;; 2 - string-append-map
(define (string-append-map xs suffix)
  (map (lambda (start) (string-append start suffix)) xs))

;; 3 - list-nth-mod
(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4 - stream-for-n-steps
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let* ([next (s)]
               [next-v (car next)]
               [next-s (cdr next)])
        (cons next-v (stream-for-n-steps next-s (- n 1))))))

;; 5 - funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons
                 (if (= (remainder x 5) 0)
                     (- 0 x)
                     x)
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 6 - dan-then-dog
(define dan-then-dog
  (letrec ([dog (lambda () (cons "dan.jpg" dan))]
           [dan (lambda () (cons "dog.jpg" dog))])
    dog))

;; 7 - stream-add-zero
(define (stream-add-zero s)
  (lambda ()
    (let* ([next (s)]
          [next-v (car next)]
          [next-s (cdr next)])
      (cons (cons 0 next-v) (stream-add-zero next-s)))))

;; 8 - cycle-lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons
                 (cons (list-nth-mod xs x) (list-nth-mod ys x))
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;; 9 - vector-assoc
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos)
                (cond [(= pos (vector-length vec)) #f]
                      [(not (pair? (vector-ref vec pos))) (f (+ pos 1))]
                      [(equal? v (car (vector-ref vec pos))) (vector-ref vec pos)]
                      [#t (f (+ pos 1))]))])
    (f 0)))

;; 10 - caches-assoc
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-pos 0]
           [update-cache (lambda (v)
                           (vector-set! cache cache-pos v)
                           (set! cache-pos
                                 (if (= (+ cache-pos 1) n)
                                     0
                                     (+ cache-pos 1))))])
    
    (lambda (v)
      (let ([v-from-cache (vector-assoc v cache)])
        (cond
         [v-from-cache v-from-cache]
         [#t (let ([result (assoc v xs)]) (update-cache result) result)])))))                  
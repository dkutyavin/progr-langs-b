#lang racket
; test utils
(define ones (lambda () (cons 1 ones)))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let* ([next (s)]
               [next-v (car next)]
               [next-s (cdr next)])
        (cons next-v (stream-for-n-steps next-s (- n 1))))))

; 1 - palindromic
(define (palindromic xs)
  (letrec ([palindromic-sum (lambda (pos)
                              (+
                               (list-ref xs pos)
                               (list-ref xs (- (length xs) pos 1))))]
           [f (lambda (pos)
                (if (= pos (length xs))
                    null
                    (cons (palindromic-sum pos) (f (+ pos 1)))))])
    (f 0)))

; 2 - fibonacci
; (stream-for-n-steps fibonacci n)
(define fibonacci
  (letrec ([cache null]
           [f (lambda (x)
                     (let ([result
                            (cond
                              [(= x 0) 0]
                              [(= x 1) 1]
                              [#t (+ (cdr (assoc (- x 2) cache)) (cdr (assoc (- x 1) cache)))])])
                       (set! cache (cons (cons x result) cache))
                       (cons result (lambda () (f (+ 1 x))))))])
    (lambda () (f 0))))

; 3 - stream-until
(define (stream-until f s)
  (let* ([next (s)]
         [next-v (car next)]
         [next-s (cdr next)])
    (if (f next-v)
        (stream-until f next-s)
        next-v)))

; 4 - stream-map
(define (stream-map f s)
  (let* ([next (s)]
         [next-v (car next)]
         [next-s (cdr next)])
    (lambda () (cons
                (f next-v)
                (stream-map f next-s)))))
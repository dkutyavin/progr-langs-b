#lang racket
; test utils
(define zeros (lambda () (cons 0 zeros)))
(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define negs (lambda () ((stream-map (lambda (x) (- x)) nats))))

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

; 5 - stream-zip
(define (stream-zip s1 s2)
  (let ([next1 (s1)]
        [next2 (s2)])
    (lambda () (cons
                (cons (car next1) (car next2))
                (stream-zip (cdr next1) (cdr next2))))))

; 7 - interleave
; (stream-for-n-steps (interleave (list nats negs zeros)) 10)
(define (interleave ss)
  (let* ([current (car ss)]
         [next (current)])
    (lambda () (cons
                (car next)
                (interleave (append
                             (cdr ss)
                             (cons (cdr next) null)))))))

; 8 - pack
(define (pack s n)
  (letrec ([next-s null]
           [get-n (lambda (acc-s acc-n)
                    (cond
                      [(= acc-n 0) (set! next-s ((lambda () (pack acc-s n)))) null]
                      [#t
                       (let ([next (acc-s)])
                          (cons
                           (car next)
                           (get-n (cdr next) (- acc-n 1))))]))])
    (lambda () (cons (get-n s n) next-s))))

; sqrt-stream 
(define (sqrt-stream n)
  (letrec ([x n]
           [calc (lambda (n) (/ (+ x (/ n x)) 2))]
           [stream (lambda (n)
                     (cons
                      (calc n)
                      (lambda () (stream (calc n)))))])
    (lambda () (stream x))))
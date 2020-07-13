#lang racket

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

(define test-btree
  (btree-node 5
   (btree-node 2 (btree-leaf) (btree-leaf))
   (btree-node 7
               (btree-node 6 (btree-leaf) (btree-leaf))
               (btree-node 10
                           (btree-leaf)
                           (btree-node 12 (btree-leaf) (btree-leaf))))))


(define (tree-height bt)
  (cond [(btree-leaf? bt) 0]
        [(btree-node? bt)
         (let ([left-height (tree-height (btree-node-left bt))]
               [right-height (tree-height (btree-node-right bt))])
           (+ 1 (if (> left-height right-height) left-height right-height)))]
        [#t (error "Incorrect btree:" bt)]))

(define (sum-tree bt)
  (cond [(btree-leaf? bt) 0]
        [(btree-node? bt) (+ (btree-node-value bt)
                         (sum-tree (btree-node-left bt))
                         (sum-tree (btree-node-right bt)))]
        [#t (error "Incorrect btree:" bt)]))

(define (prune-at-v bt v)
  (cond [(btree-leaf? bt) bt]
        [(btree-node? bt)
         (if (equal? (btree-node-value bt) v)
             (btree-leaf)
             (btree-node (btree-node-value bt)
                         (prune-at-v (btree-node-left bt) v)
                         (prune-at-v (btree-node-right bt) v)))]
        [#t (error "Incorrect btree:" bt)]))
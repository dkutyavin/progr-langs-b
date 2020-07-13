#lang racket

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)
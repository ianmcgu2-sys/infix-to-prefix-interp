#lang racket

(define (in->pre exp)
  (cond
    [(number? exp) exp]
    [(and (symbol? exp)
          (not (or (symbol=? '+ exp) (symbol=? '* exp)))) exp]
    [(empty? exp) (error "bad expression")]
    [(and (empty? (rest exp))
          (or (eq? '+ (first exp)) (eq? '* (first exp))))
     (error "bad expression")]
    [(and (list? (first exp)) (empty? (rest exp))) (in->pre (first exp))]
    [(and (number? (first exp)) (empty? (rest exp))) (first exp)]
    [(list? exp) (in->pre0 (peel exp))]))

(define (peel exp)
  (cond
    [(empty? exp) '()]
    [(list? (first exp))
     (cons (in->pre (first exp)) (peel (rest exp)))]
    [else (cons (first exp) (peel (rest exp)))]))

(define (in->pre0 exp)
  (in->pre-h exp '()))

(define (in->pre-h L acc)
  (cond
    [(empty? (rest L)) (add+ (reverse (cons (first L) acc)) acc)]
    [(eq? '+ (second L))
     (in->pre-h (rest (rest L)) (cons (first L) acc))]
    [(eq? '* (second L))
     (in->pre-hh L '() acc)]
    [else (error "bad expression")]))

(define (in->pre-hh L acc2 acc)
  (cond
    [(empty? (rest L))
     (add+ (acc+ (cons '* (reverse (cons (first L) acc2))) acc) acc)]
    [(eq? '* (second L))
     (in->pre-hh (rest (rest L)) (cons (first L) acc2) acc)]
    [else (in->pre-h (rest (rest L)) (cons (cons '* (reverse (cons (first L) acc2))) acc))]))

(define (add+ x acc)
  (if (empty? acc) x (cons '+ x)))
(define (acc+ x acc)
  (if (empty? acc) x (reverse (cons x acc))))
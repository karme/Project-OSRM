;; todo: nearly identical to huge-sparse-bitmap
(define-module node-pos-mmap
  (use srfi-1)
  (use sxml.adaptor) ;; for assert
  (use util.list)
  (use mmap)
  (use c-wrapper)
  (use gauche.sequence)
  (export node-pos-map-open
          node-pos-map-close
          node-pos-map-sync
          node-pos-map-set!
          node-pos-map-get))

(select-module node-pos-mmap)

(define (node-pos-map-open filename
                           :key
                           (max-size (ash 1 35))
                           (rw-mode :write)
                           ;; useless
                           (slot-size 64)
                           (cache-size 4096)
                           )
  (receive (p sync)
      (mmap-cast filename
                 (* max-size (c-sizeof <c-int>) 2)
                 #?=(eq? rw-mode :write)
                 (lambda(x)
                   (cast (ptr <c-int>) x)))
    
    (define (node-pos-get id :optional (default #f))
      (assert (< id max-size))
      (let1 r (map (compose
                    (lambda(x)
                      (cond [(= x 0)
                             +nan.0]
                            [(> x 0)
                             (let1 r (inexact (/ (- x 1) 10000000))
                               (assert (<= r 180))
                               r)]
                            [else
                             (let1 r (inexact (/ x 10000000))
                               (assert (>= r -180))
                               r)]))
                    (cut cast <number> <>))
                   (list (deref (c-ptr+ p (* id 2)))
                         (deref (c-ptr+ p (+ (* id 2) 1)))))
        (if (or (nan? (car r)) (nan? (cadr r)))
          default
          r)))
    
    (define (node-pos-set! id x y)
      (assert (< id max-size))
      (for-each-with-index (lambda(i n)
                             (assert (number? n))
                             (set! (ref (deref (c-ptr+ p (+ (* id 2) i))))
                                   (cond [(nan? n)
                                          0]
                                         [(>= n 0)
                                          (assert (<= n 180))
                                          (round->exact (+ (* n 10000000) 1))]
                                         [else
                                          (assert (and (< n 0) (>= n -180)))
                                          (round->exact (* n 10000000))])))
                           (list x y)))
    (let1 r `((set!  . ,node-pos-set!)
              (get   . ,node-pos-get)
              (sync  . ,sync)
              (close . ()))
      (set-cdr! (assq 'close r)
                (lambda()
                  ((assoc-ref r 'sync))
                  (set-cdr! (assq 'set! r) (lambda _ (error "closed")))
                  (set-cdr! (assq 'get r) (lambda _ (error "closed")))))
      r)))

(define (node-pos-map-set! np . l)
  (apply (assoc-ref np 'set!) l))

(define (node-pos-map-get np . l)
  (apply (assoc-ref np 'get) l))

(define (node-pos-map-close np)
  ((assoc-ref np 'close)))

(define (node-pos-map-sync np)
  ((assoc-ref np 'sync)))

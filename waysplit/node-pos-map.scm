;;; huge sparse (persistent) node position map
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;todos:
;;;- nearly identical to huge-sparse-bitmap
;;;

(define-module node-pos-map
  (use srfi-1)
  (use dbm.gdbm)
  (use gauche.collection)
  (use gauche.sequence)
  (use sxml.adaptor) ;; for assert
  (use lru-cache)
  (use util.list)
  (use gauche.uvector)
  (export node-pos-map-open
          node-pos-map-close
          node-pos-map-sync
          node-pos-map-set!
          node-pos-map-get))

(select-module node-pos-map)

;; note: must be a otherwise invalid key!
;; all allowed key decoding routings must fail!
;; => disallow bin and ber as key encoding for now (see below)
(define-constant *meta-key* "_M")

(define (real-pair? x)
  (and (pair? x) (not (list? x))))

(define (slot? s)
  (and (real-pair? s)
       (exact? (car s))
       (f64vector? (cdr s))))

(define (node-pos-map-open filename
                           :key
                           (slot-size 64)
                           (cache-size 4096)
                           (rw-mode :write)
                           ;; useless
                           (max-size #f)
                           )
  (define enc
    (let1 buf (make-s32vector (* slot-size 2))
      (lambda(v)
        (assert (f64vector? v))
        (assert (= (f64vector-length v) (s32vector-length buf)))
        (for-each-with-index (lambda(i n)
                               (assert (number? n))
                               (set! (s32vector-ref buf i)
                                     (cond [(nan? n)
                                            0]
                                           [(>= n 0)
                                            (assert (<= n 180))
                                            (round->exact (+ (* n 10000000) 1))]
                                           [else
                                            (assert (and (< n 0) (>= n -180)))
                                            (round->exact (* n 10000000))])))
                             v)
        (u8vector->string (uvector-alias <u8vector> buf)))))

  (define (dec s)
    (assert (string? s))
    (map-to <f64vector>
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
            (uvector-alias <s32vector> (string->u8vector s))))

  (let1 v (make-f64vector (* slot-size 2) 0)
    (set! (ref v 0) 1)
    (set! (ref v 1) -1)
    (set! (ref v 2) 180)
    (set! (ref v 3) -180)
    (assert (equal? v (dec (enc v)))))

  (let ((db (dbm-open <gdbm> :path filename :rw-mode rw-mode)))
    ;; load options from db if it already exists
    (if-let1 meta (dbm-get db *meta-key* #f)
      (receive (s)
          (apply values (read-from-string meta))
        (unless (= s slot-size)
          ;; todo: we really should use db meta data as defaults
          (error "options don't match db"))))
    ;; save options to db
    (when (not (eq? rw-mode :read))
      (dbm-put! db *meta-key* (write-to-string (list slot-size))))

    (let ((read-slot-value (lambda(k)
                             (assert (exact? k))
                             (if-let1 v (dbm-get db (number->string k 32) #f)
			       (dec v)
                               (make-f64vector (* slot-size 2) +nan.0))))
          (write-slot-value (lambda(k v)
                              (assert (exact? k))
                              (assert (f64vector? v))
                              (assert (= (size-of v) (* slot-size 2)))
                              (dbm-put! db (number->string k 32) (enc v)))))
      (let1 cache (if (> cache-size 0)
                    (make-lru-cache read-slot-value write-slot-value :cache-size cache-size)
                    '())

        (define read-slot
          (let1 get (assoc-ref cache 'get read-slot-value)
            (lambda(sid)
              (cons sid (get sid)))))

        (define write-slot!
          (let1 put! (assoc-ref cache 'put! write-slot-value)
            (lambda(s)
              (put! (car s) (cdr s)))))

        (define (slot-set! s r x y)
          (assert (slot? s))
          (f64vector-set! (cdr s) (* r 2) x)
          (f64vector-set! (cdr s) (+ (* r 2) 1) y)
          s)

        (define (node-pos-set! id x y)
          (receive (q r) (quotient&remainder id slot-size)
            (write-slot!
             (slot-set!
              (read-slot q) r x y)))
          (undefined))

        (define (slot-get slot r default)
          (assert (slot? slot))
	  (let* ((r2 (* r 2))
		 (x (f64vector-ref (cdr slot) r2))
		 (y (f64vector-ref (cdr slot) (+ r2 1))))
	    (if (or (nan? x) (nan? y))
		default
		(list x y))))

        (define (node-pos-get id :optional (default #f))
          (receive (q r) (quotient&remainder id slot-size)
            (slot-get (read-slot q) r default)))

        (define sync
          (let1 cache-sync (assoc-ref cache 'sync (lambda ()))
            (lambda()
              (cache-sync)
              ;; todo: generic dbm api is missing sync
              ;; how to sync db then?
              ;; we can only close and re-open?
              ;; for now use gdbm specific api
              (assert (ref db 'gdbm-file))
              (gdbm-sync (ref db 'gdbm-file)))))

        (let1 r `((set!  . ,node-pos-set!)
                  (get   . ,node-pos-get)
                  (sync  . ,sync)
                  (close . ()))
          (set-cdr! (assq 'close r)
                    (lambda()
                      ((assoc-ref r 'sync))
                      (set-cdr! (assq 'set! r) (lambda _ (error "closed")))
                      (set-cdr! (assq 'get r) (lambda _ (error "closed")))
                      (dbm-close db)))
          r)))))

(define (node-pos-map-set! np . l)
  (apply (assoc-ref np 'set!) l))

(define (node-pos-map-get np . l)
  (apply (assoc-ref np 'get) l))

(define (node-pos-map-close np)
  ((assoc-ref np 'close)))

(define (node-pos-map-sync np)
  ((assoc-ref np 'sync)))

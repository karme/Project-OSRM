#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; calculate way splits
;;;
;;; Copyright (C) 2013 Jens Thiele <karme@karme.de>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(use file.util)
(use huge-sparse-bitmap)
(use gauche.sequence)
(use sxml.adaptor) ;; for assert
(use sxml.sxpath)
(use sxml.tools)
(use dbm.gdbm)
(use srfi-19)

(define (string->exact x)
  (let1 n (string->number x)
    (assert n)
    (assert (exact? n))
    n))

(define way-id (car-sxpath '(@ id *text*)))
(define way-nodes (compose (cute map string->exact <>) (sxpath '(nd @ ref *text*))))

(define (at-least-two? x)
  (assert (list? x))
  (not (or (null? x)
           (null? (cdr x)))))

;; note: parallel-pipe expects complete packets and one for each input packet
(define (write-line x)
  (write x)
  (newline)
  (flush))

(define (main args)
  (let-optionals* (cdr args) ((node-bitmap "bitmap.dbm")
                              (cache-size "4096"))
    (let ((bm (make-huge-sparse-bitmap node-bitmap
                                       :cache-size (string->number cache-size)
                                       :rw-mode :read)))
      (until (read) eof-object? => expr
             (assert (eq? (car expr) 'way))
	     ;; note: we have to output a line for each input line for parallel-pipe
	     (write-line
	      (let* ((nodes (way-nodes expr))
                     (splits (and (at-least-two? nodes)
                                  (filter (cute huge-sparse-bitmap-get-bit bm <>)
                                          (subseq nodes 1 -1)))))
                (cond [(and splits
                            (not (null? splits)))
                       ;; have to split that way
                       (let* ((wn (list))
                              (new-ways (list))
                              (do-split (lambda()
                                          (let ((en (car wn)))
                                            ;; keep track of splits
                                            (push! new-ways (reverse wn))
                                            (set! wn (list en))))))
                         ;; walk along the way and split it
                         ;; todo: improve
                         (push! wn (car nodes))
                         (for-each (lambda(n)
                                     (assert (not (null? wn)))
                                     (push! wn n)
                                     (when (member n splits)
                                       (do-split)))
                                   (cdr nodes))
                         ;; last node might not be a real node
                         ;; => we might have something to write
                         (when (at-least-two? wn)
                           (do-split))
                         ;; output splits
                         (cons (string->exact (sxml:attr expr 'id)) (reverse new-ways)))]
                      [else
                       '()]))))))
  0)


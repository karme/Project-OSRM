#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; store way splits
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
(use gauche.sequence)
(use sxml.adaptor) ;; for assert
(use dbm.gdbm)

(define *id* 0)

(define (next-id)
  (inc! *id*))

(define (string->exact x)
  (let1 n (string->number x)
    (assert n)
    (assert (exact? n))
    n))

(define (at-least-two? x)
  (assert (list? x))
  (not (or (null? x)
           (null? (cdr x)))))

(define (main args)
  (assert (at-least-two? args))
  (set! *id* (string->exact (cadr args)))
  (let-optionals* (cddr args) ((way-splits "way-splits.dbm"))
    (let ((way-splits (dbm-open <gdbm> :path way-splits :rw-mode :write)))
      (until (read) eof-object? => expr
             (when (at-least-two? expr)
               (dbm-put! way-splits
                         (number->string (car expr))
                         (write-to-string (map (lambda(x)
                                                 (cons (next-id) x))
                                               (cdr expr))))))
      (dbm-close way-splits)))
  0)

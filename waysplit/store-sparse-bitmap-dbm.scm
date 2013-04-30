#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; store sparse bitmap
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
(use huge-sparse-bitmap)
;;(use sxml.adaptor) ;; for assert
(use srfi-19)

;; disable assert
(define-macro (assert e) )

(define (string->exact x)
  (let1 n (string->number x)
    (assert n)
    (assert (exact? n))
    n))

(define-syntax info
  (syntax-rules ()
    ((_ ?form)
     (format/ss (current-error-port)
                "~a~a pid=~s INFO: ~a"
                (let1 si (debug-source-info '?form)
                  (or (and si
                           (string-append (car si)
                                          ":"
                                          (x->string (cadr si)) ": " ))
                      ""))
                (date->string (current-date) "~1 ~T") ;; .~N~z")
                (sys-getpid)
                ?form))))

(define (main args)
  (let-optionals* (cdr args) ((filename "bitmap.dbm")
                              (cache-size "1")) ;; doesn't matter in this setting (input is sorted)
    (let ((bm (make-huge-sparse-bitmap filename
                                       :cache-size (string->number cache-size)))
          (bits 0))
      (until (read-line) eof-object? => x
             (huge-sparse-bitmap-set-bit! bm (string->exact x) #t)
             (assert (huge-sparse-bitmap-get-bit bm (string->exact x)))
             (inc! bits))
      (huge-sparse-bitmap-sync bm)
      (info (format #f "stored ~s bits\n" bits))))
  0)

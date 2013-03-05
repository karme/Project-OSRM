#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; find used nodes
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
(use sxml.sxpath)
(use sxml.adaptor) ;; for assert

(define (string->exact x)
  (let1 n (string->number x)
    (assert (and n (exact? n)))
    n))

(define way-nodes (compose (cute map string->exact <>)
                           (sxpath '(nd @ ref *text*))))

(define (main args)
  (until (read) eof-object? => expr
         (when (eq? (car expr) 'way)
           (for-each print (way-nodes expr))))
  0)

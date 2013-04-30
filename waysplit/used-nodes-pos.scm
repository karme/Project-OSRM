#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; extract used nodes positions
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
(use sxml.adaptor) ;; for assert
(use sxml.sxpath)
(use sxml.tools)
(use dbm.gdbm)
(use pipe)

(define (string->exact x)
  (let1 n (string->number x)
    (assert (and n (exact? n)))
    n))

(define node-x (compose string->number (cute sxml:attr <> 'lon)))
(define node-y (compose string->number (cute sxml:attr <> 'lat)))
(define node-id (compose string->exact (cute sxml:attr <> 'id)))

(define (main args)
  ;;#?=(get-pipe-buffer-size (current-output-port))
  (set-pipe-buffer-size! (current-output-port) (ash 1 20))
  ;;#?=(get-pipe-buffer-size (current-output-port))
  (let-optionals* (cdr args) ((used-nodes "used-nodes.dbm")
                              (cache-size "8192"))
    (let ((used-nodes (make-huge-sparse-bitmap used-nodes
                                               :cache-size (string->number cache-size)
                                               :rw-mode :read)))
      (until (read) eof-object? => expr
             (assert (eq? (car expr) 'node))
             (let1 id (node-id expr)
               (when (huge-sparse-bitmap-get-bit used-nodes id)
                 (let ((x (node-x expr))
                       (y (node-y expr)))
                   (assert (and (number? x) (number? y)))
                   (print id " " x " " y)))))))
  0)

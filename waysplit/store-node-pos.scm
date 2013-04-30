#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; store node positions
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

(use node-pos-map)

(define (main args)
  (let-optionals* (cdr args) ((node-pos-file "node-pos.dbm")
                              (max-id (number->string (ash 1 35))))
    (let ((node-pos (node-pos-map-open node-pos-file
                                       ;; doesn't really matter in this setting
                                       ;; (input should be sorted)
                                       :cache-size 1
                                       :max-size (string->number max-id)
                                       )))
      (until (read-line) eof-object? => line
	     (let1 l (string-split line #\space)
               (apply node-pos-map-set! (cons node-pos (map string->number l)))))
      (node-pos-map-close node-pos)))
  0)

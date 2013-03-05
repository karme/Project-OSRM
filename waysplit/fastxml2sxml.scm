#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; convert osm xml to sxml stream
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
(use srfi-1) ;; for older gauche (filter was in srfi-1)
(use xml2sxml)
(use sxml.adaptor) ;; for assert
(define whitespace?
  (compose boolean #/^\s*$/))

(define (whitespace-filter expr)
  (assert (list? expr))
  (cons (car expr)
        (filter (lambda(x)
                  (or (not (string? x))
                      (not (whitespace? x))))
                (cdr expr))))

(define (main args)
  (let* ((reader (make-xml-reader (current-input-port)))
         (handle-node (lambda()
                        (let1 expr (xml-reader-node reader)
                          (when expr
                            (write (whitespace-filter expr))
                            (newline))))))
    (assert (and (xml-reader-read reader)
                 (xml-reader-read reader)))
    (handle-node)
    (while (xml-reader-next reader)
      (handle-node))
    (close-xml-reader reader))
  0)

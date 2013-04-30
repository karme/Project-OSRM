#| -*- mode: scheme; coding: utf-8; -*- |#
;;; simple osrm API client
;;; Copyright (C) 2012 Jens Thiele <karme@karme.de>
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

(define-module osrm-client
  (use rfc.uri)
  (use rfc.http)
  (use util.list)
  (use gauche.sequence)
  (use srfi-14)
  (use rfc.json)
  (export osrm-route
          osrm-pick))

(select-module osrm-client)

;; note:
;; if http-compose-query would support noescape or osrm would support
;; escapes we could remove that
(define (parameters->string ps)
  (define (esc x)
    (uri-encode-string (x->string x)
                       :noescape (char-set-union *rfc2396-unreserved-char-set* #[,])))

  (string-join (map (lambda(p)
                      #`",(esc (car p))=,(esc (cadr p))")
                    ps) "&"))

(define (points->parameters points)
  (map
   (lambda(p)
     (list 'loc (format #f "~a,~a" (cdr p) (car p))))
   points))

(define (tee f x)
  (with-output-to-file f
    (lambda()
      (write x)
      (newline)))
  x)

(define (osrm-route points . args)
  (let-optionals* args ((params '())
                        (service '("localhost:5000" "/viaroute")))
    (let ((server      (car service))
          (request-uri (cadr service))
          (params (append params
                          (points->parameters points)
                          '((instructions true)
                            (compression false)
                            (alt false)))))
      (receive (status headers body)
          ;; note: osrm doesn't support post?!
          (http-get server
                    (format #f
                            (string-append request-uri "?~a")
                            (parameters->string params)))
        (parse-json-string body)))))

;; todo: nearly identical to osrm-route
(define (osrm-pick point . args)
  (let-optionals* args ((params '())
                        (service '("localhost:5000" "/nearest")))
    (let ((server      (car service))
          (request-uri (cadr service))
          (params (append params
                          (points->parameters (list point)))))
      (receive (status headers body)
          ;; note: osrm doesn't support post?!
          (http-get server
                    (format #f
                            (string-append request-uri "?~a")
                            (parameters->string params)))
        (parse-json-string body)))))

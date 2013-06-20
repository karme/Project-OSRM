#| -*- mode: scheme; coding: utf-8; -*- |#
;;;
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
;;;
(define-module routing
  (use www.cgi)
  (use sxml.serializer)
  (use sxml.sxpath)
  (use gauche.sequence)
  (use srfi-1)
  (use util.list)
  (export route-error?
	  route-error-code
	  <route-error>
	  render-xml
	  render-sxml
	  fatal-error?
	  <fatal-error>
          linref-append
          route-append
          alist->linrefelem
	  ))

(select-module routing)

(load "debug.scm")

(define-condition-type <route-error> <error>
  route-error?
  (code route-error-code))

(define-condition-type <fatal-error> <error>
  fatal-error?)

(define (route-error->cgi-header e content-type)
  (case (route-error-code e)
    [(time-limit-reached)
     (cgi-header :content-type content-type
		 :status "408 Request Timeout"
		 :Retry-After 120)]
    [else
     (cgi-header :content-type content-type
		 :status "400 Bad Request")]))

(define (route-error->sxml e)
  `(error (@ (scm ,(write-to-string `(error <route-error> :code ',(route-error-code e)))))))

(define (render-sxml thunk)
  (guard (e
	  [(fatal-error? e)
	   (raise e)]
	  [(route-error? e)
	   (list (route-error->cgi-header e "application/x-sxml")
		 (write-to-string (route-error->sxml e)))]
	  [(<error> e)
	   ;; unknown generic error
	   (list (cgi-header :content-type "application/x-sxml"
			     :status "400 Bad Request")
		 (write-to-string `(error (@ (scm ,(write-to-string `(error ,(string-append (ref e 'message)
											    (x->string (class-of e))))))))))]
	  [else
	   ;; unknown generic error
	   (list (cgi-header :content-type "application/x-sxml"
			     :status "400 Bad Request")
		 (write-to-string `(error (@ (scm ,(write-to-string `(error ,(string-append "unknown error"
											    (x->string (class-of e))))))))))])
	 (list (cgi-header :content-type "application/x-sxml")
	       (write-to-string (thunk)))))

(define (sxml->xml sxml)
  (srl:sxml->xml ;; -noindent
   `(*TOP*
     (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
     ,sxml)))

(define (render-xml thunk)
  (guard (e
	  ;; todo: catch all errors except fatal-error
	  [(route-error? e)
	   (list (route-error->cgi-header e "application/xml")
		 (sxml->xml (route-error->sxml e)))])
	 (list (cgi-header :content-type "application/xml")
	       (sxml->xml (thunk)))))

;; todo: also in ...
(define-macro (debug-assert e)
  `(when (not ,e)
     (error "Debug assertion failed: " ,(x->string e))))

;; todo: also in ...
(define (attr-alist x)
  (map (lambda(kv)
	 (cons (car kv) (cadr kv)))
       (sxml:attr-list x)))

;; todo: also in ...
(define linrefelem->alist attr-alist)

;; todo: also in ...
(define linref-length
  (compose x->number (car-sxpath '(@ total-length *text*))))

;; todo: also in ...
(define (x->symbol x)
  (if (symbol? x)
      x
      (string->symbol x)))

;; todo: also in ...
(define (alist->sxml-attr alist)
  `(@ . ,(map (lambda(p)
		(debug-assert (pair? p))
		(debug-assert (not (pair? (cdr p))))
		(list (x->symbol (car p)) (x->string (cdr p))))
	      alist)))

(define (alist->linrefelem x)
  `(linrefelem ,(alist->sxml-attr x)))

(define (linref-append . l)
  (cond [(null? l)
	 l]
	[(= (size-of l) 1)
	 (car l)]
	[else
	 (let1 new-total-length (apply + (map linref-length l))
	       `(linref (@ (total-length ,(x->string new-total-length)))
			;; todo: 
			;; - merge like in shortest-path
			;; - linref code in shortest-path is quite similar
			. ,(fold2 (lambda(new ret offset)
				    (let1 sub-length (linref-length new)
					  (values (append ret
							  (map (lambda(linref-elem)
								 (let* ((l (alist-copy (linrefelem->alist linref-elem)))
									(adjust (lambda(x)
										  (x->string
										   (/. (+ offset
											  (* (x->number (assoc-ref l x))
											     sub-length))
										       new-total-length)))))
								   ;; adjust from, to
								   ;; note: set! ok
								   (assoc-set! l 'from (adjust 'from))
								   (assoc-set! l 'to (adjust 'to))
								   (alist->linrefelem l)))
							       ((sxpath '(linrefelem)) new)))
						  (+ offset sub-length))))
				  '()
				  0
				  l)))]))

(define (route-append params l)
  ;; todo: multiple routes?!, topo, crossings, ...
  ;; s.a. mixed-path
  `(itdRoute . ,(append ((sxpath '(// itdPartialRoute)) l)
                        (list (apply linref-append ((sxpath '(// linref)) l)))
                        ;; crossings
                        (if (cgi-get-parameter "crossings" params :default #f)
                          (list
                           `(crossings . ,((sxpath '(// crossing)) l)))
                          '()))))

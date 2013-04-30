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
(define-module partial-route
  (use srfi-1)
  (use srfi-13)
  (use gauche.sequence)
  (use sxml.sxpath)
  (use sxml.tools)
  (export make-partial-route
          partial-route-coords
	  partial-route-length
	  partial-route-adjust-path
	  partial-route-from
	  partial-route-to
          partial-route-duration))

(select-module partial-route)

;; todo: also in ...
(define-macro (debug-assert e)
  `(when (not ,e)
     (error "Debug assertion failed: " ,(x->string e))))

(define partial-route-from
  (if-car-sxpath '((itdPoint (@ (equal? (usage "departure")))))))

(define partial-route-to
  (if-car-sxpath '((itdPoint (@ (equal? (usage "arrival")))))))

;; note: in seconds!
(define partial-route-duration
  (compose (cut * 60 <>) x->number (car-sxpath '(@ timeMinute *text*))))

(define (point-coords p)
  (let ((x ((if-car-sxpath '(@ x *text*)) p))
	(y ((if-car-sxpath '(@ y *text*)) p)))
    (if (and x y)
	(map string->number (list x y))
	#f)))

(define (partial-route-straight-line pr)
  (let1 r #?=(map point-coords
		  (list (partial-route-from pr)
			(partial-route-to pr)))
	(if (every boolean r)
	    r
	    '())))

(define (partial-route-path pr)
  (if-let1 r ((if-car-sxpath '(itdPathCoordinates itdCoordinateString *text*)) pr)
	   r
	   ""))

;; note: path may contain only one point!
(define (path->polyline s)
  (debug-assert (string? s))
  (if (string-null? s)
    '()
    (map (lambda(sp)
           (map x->number (string-split sp #\,)))
         (string-split s #\space))))

;; todo:
;; - add "hidden" footpaths in itdFootPathInfo
(define (partial-route-coords pr)
  (let ((path (partial-route-path pr)))
    (cond [(not (string-null? path))
	   (path->polyline path)]
	  [else
	   (partial-route-straight-line pr)])))

(define (partial-route-length pr)
  (let1 coords (partial-route-coords pr)
	(debug-assert (and (list? coords)
			   (not (null? coords))))
	(let1 lastp (last coords)
	      (debug-assert (= (size-of lastp) 4))
	      (ref lastp 3))))

(define (polyline->path l)
  (debug-assert (list? l))
  ;; (debug-assert (or (null? l)
  ;; 		    (and (list? (car l))
  ;; 			 (number? (caar l)))))
  (string-join
   (map (lambda(p)
	  (string-join (map x->string p) ","))
	l)
   " "))

(define (make-path-coordinates path)
  `(itdPathCoordinates
    (coordEllipsoid "WGS84")
    (coordType "GEO_DECIMAL")
    (itdCoordinateReductionList)
    (itdCoordinateString (@ (ts "&#x20;") (decimal ".") (cs ","))
			 ,path)))

;; note ugly: in minutes!
(define (make-partial-route polyline duration)
  `(itdPartialRoute (@ (timeMinute ,(x->string duration)))
		    ,(make-path-coordinates (polyline->path polyline))))

(define (adjust-cs cs new)
  (debug-assert (equal? (car cs) 'itdCoordinateString))
  (sxml:change-content cs (list new)))

;; todo:
;; - there should be a more elegant solution?
(define (partial-route-adjust-path pr polyline)
  (let ((new (polyline->path polyline)))
    (debug-assert (equal? (car pr) 'itdPartialRoute))
    (debug-assert (string? new))
    (cons (car pr)
	  (append
	   ((sxpath '((not@ itdPathCoordinates))) pr)
	   (if-let1 pc ((if-car-sxpath '(itdPathCoordinates)) pr)
             (list
              (cons 'itdPathCoordinates
                    (append ((sxpath '((not@ itdCoordinateString))) pc)
                            (list (adjust-cs ((if-car-sxpath '(itdCoordinateString)) pc) new)))))
             (list
              #?=(make-path-coordinates new)))))))

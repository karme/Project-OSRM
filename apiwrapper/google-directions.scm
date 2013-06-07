#| -*- mode: scheme; coding: utf-8; -*- |#
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
(define-module google-directions
  (use format-json) ;; todo: replace with rfc.json
  (use srfi-1)
  (use srfi-13)
  (use gauche.sequence)
  (use util.list)
  (use www.cgi)
  (use sxml.sxpath)
  (use partial-route)
  (use routing)
  (use google-polyline)
  (export google-directions-query->track
          google-directions-v3-out))

(select-module google-directions)

(load "debug.scm")

;; todo: also in ...
(define-macro (debug-assert e)
  `(when (not ,e)
     (error "Debug assertion failed: " ,(x->string e))))

(define json-format-number (format-number-fixed-point 4))

(define (json-display-number x)
  (display (json-format-number x)))

(define to-json (cut ->jsonf <> `((number . ,json-display-number))))

;; todo: also in ...

;; input: list (x1 x2 ... xn)
;; output: list of pairs where consecutive elements form one pair:
;; ((x1 . x2) (x3 . x4) ... (xn-1 . xn))
(define (group-pairwise l)
  (debug-assert (even? (size-of l)))
  (fold-right (lambda(n o) (if (or (null-list? o)
                                   (pair? (car o)))
                             (cons n o)
                             (cons (cons n (car o)) (cdr o))))
              (list)
              l))

;; todo: performance will be bad
(define (google-directions-query->track x)
  (fold-right
   (lambda(n o)
     ;; note: google coordinates are y,x
     (cons (cdr n) (cons (car n) o)))
   (list)
   (group-pairwise
    (map x->number
         (string-split
          (string-drop-right
           (regexp-replace #/^from: ?/
                           (regexp-replace-all #/ ?to: ?/
                                               (string-append x ",")
                                               ",")
                           "")
           1)
          ",")))))

;; todo: also in, bad name
(define (first-pair x)
  (cons (car x) (cadr x)))

;; todo: also in, ...
(define (error->backtrace e)
  (with-output-to-string
    (lambda()
      (with-error-to-port (current-output-port)
        (lambda()
          (report-error e))))))

(define (tee f x)
  (with-output-to-file f
    (lambda()
      (write x)
      (newline)))
  x)

(define (sjson->sxml sjson)
  (if (string? sjson)
    (list sjson)
    (append
     `((@ . ,(map (lambda(kv)
                    (list (car kv)
                          (if (number? (cdr kv))
                            (json-format-number (cdr kv))
                            (x->string (cdr kv)))))
                  (filter (lambda(kv) (and (pair? kv)
                                           (not (or (list? (cdr kv))
                                                    (vector? (cdr kv))))))
                          sjson))))
     (append-map
      (lambda(kv)
        (if (list? (cdr kv))
          (list (cons (car kv) (sjson->sxml (cdr kv))))
          (map (cut cons (car kv) <>)
               (map (cut sjson->sxml <>)
                    (cdr kv)))))
      (filter (lambda(kv) (and (pair? kv)
                               (or (list? (cdr kv))
                                   (and (vector? (cdr kv))
                                        (> (size-of (cdr kv)) 0)))))
              sjson)))))

;; s.a.
;; http://code.google.com/apis/gdata/docs/json.html
;; google rules related to this are somewhat stupid
;; (see link example)
(define (sxml->sjson array-elements sxml)
  (append
   (map (lambda(kv)
          (cons (car kv) (cadr kv)))
        (sxml:attr-list sxml))
   (receive (array-childs attr-childs)
       (partition (lambda(c)
                    (and (list? c)
                         (member (car c) array-elements)))
                  (filter sxml:element?
                          (sxml:child-nodes sxml)))
     (if (and (null? array-childs)
              (null? attr-childs)
              (null? (sxml:attr-list sxml))
              (not (or (null? sxml)
                       (null? (cdr sxml)))))
       (cadr sxml) ;; hack, bug here?
       (append
        (map (lambda(c)
               (cons (car c) (sxml->sjson array-elements c)))
             attr-childs)
        (map
         (lambda(ac)
           (cons (caar ac)
                 (map-to <vector>
                         (cut sxml->sjson array-elements <>)
                         ac)))
         (group-collection array-childs :key car)))))))

(define one? (cut = 1 <>))

(define (sjson-check sjson)
  (debug-assert (or (list? sjson)
              (vector? sjson)
              (string? sjson)))
  (when (list? sjson)
    (debug-assert (every pair? sjson))
    (let1 col (group-collection sjson :key car)
      (when (not (every one? (map size-of col)))
        (error "duplicate properties:" (map caar (filter (compose not one? size-of)
                                                         col))))))
  (when (vector? sjson)
    ;; todo: every does not work with vector?!
    (debug-assert (every (lambda(x) (or (string? x) (list? x)))
                         (coerce-to <list> sjson))))
  (when (vector? sjson)
    (for-each sjson-check sjson))
  (when (list? sjson)
    (for-each sjson-check (map cdr sjson)))
  sjson)

(define (sxml-format-numbers obj)
  (cond [(sxml:element? obj)
         (append (list (car obj))
                 (if (null? (sxml:attr-list obj))
                   '()
                   `((@ . ,(map sxml-format-numbers (sxml:attr-list obj)))))
                 (map sxml-format-numbers (sxml:child-nodes obj)))]
        [(number? obj)
         (json-format-number obj)]
        [(string? obj)
         obj]
        [else
         (error "todo" obj)]))

(define (apply-filters filters sxml)
  (define ->sjson (cut sxml->sjson '(match routes warnings waypoint_order Attributes legs steps crossings froms tos info_texts) <>))
  
  (debug-assert (list? filters))
  (debug-assert (every string? filters))
  (if (null? filters)
      (->sjson sxml)
      (map-to <vector>
	      (lambda(f)
		(let1 filtered (map (lambda(match)
				      (list 'match match))
				    ((sxpath f) (sxml-format-numbers sxml)))
		      (if (null? filtered)
			  #()
			  (sjson-check (cdar (->sjson (cons '|*TOP*| filtered)))))))
	      filters)))

(define (debug-dump proc ofile)
  (lambda l
    (with-output-to-file ofile
      (lambda()
	(let1 r (apply proc l)
	      (write (list (cons (ref proc 'info) l)
			   r))
	      (newline)
	      r))
      :if-exists :append)))

;; some links:
;; https://devteam.alpserver.de/trac/tp/wiki/routing-api
;; http://code.google.com/intl/en/apis/maps/documentation/directions/
;; http://code.google.com/intl/en/apis/maps/documentation/javascript/reference.html#DirectionsResult
;; http://code.google.com/intl/de/apis/maps/documentation/javascript/examples/directions-simple.html
(define (google-style sxml coord-transform)

  (define (attr-alist x)
    (map (lambda(kv)
	   (cons (car kv) (cadr kv)))
	 (sxml:attr-list x)))

  (define (duration x)
    `(duration (@ (value ,x) (text ,#`",(round->exact (/ x 60)) min"))))

  (define (distance x)
    `(distance (@ (value ,x) (text ,#`",(round->exact x)m"))))

  (define (start-location coords)
    `(start_location (@ (lng ,(ref* coords 0 0))
			(lat ,(ref* coords 0 1)))))

  (define (end-location coords)
    `(end_location (@ (lng ,(ref (last coords) 0))
		      (lat ,(ref (last coords) 1)))))

  (define (itdDateTime dt)
    (let ((date (lambda(x)
		  (ref* ((sxpath `(itdDate @ ,x)) dt) 0 1)))
	  (time (lambda(x)
		  (ref* ((sxpath `(itdTime @ ,x)) dt) 0 1))))
      `(@ . ,(append
              (map (lambda(x) (list x (date x)))
                   '(year month day weekday))
              (map (lambda(x) (list x (time x)))
                   '(hour minute))))))

  (define (transform-pr pr)
    (let ((from  (partial-route-from pr))
	  (to    (partial-route-to pr))
	  (xyzm  (partial-route-coords pr))
	  (attr  (attr-alist pr))
	  (means-attr (attr-alist ((car-sxpath '(itdMeansOfTransport)) pr))))
      (append
       `(steps (@ (travel_mode ,(or (assoc-ref means-attr 'travel_mode)
                                    "ALPSTEIN_ROUTING"))
                  (html_instructions "todo")
                  (travel_line ,(or (assoc-ref means-attr 'name)
                                    (assoc-ref means-attr 'productName)
                                    "")))
               ,(duration (* 60 (x->number (assoc-ref attr 'timeMinute))))
               ,(distance (partial-route-length pr))
               ,(start-location xyzm)
               ,(end-location xyzm)
               ,(coord-transform xyzm))
       (if-let1 x ((if-car-sxpath '(itdDateTime)) from)
         `((travel_start ,(itdDateTime x)))
         '())
       (if-let1 x ((if-car-sxpath '(itdDateTime)) to)
         `((travel_end ,(itdDateTime x)))
         '())
       ((sxpath '(mentz_info)) pr))))

  (define (transform-crossings crossings)
    (if (null? crossings)
	crossings
	(map (lambda(e)
	       `(crossings (@ . ,(map (lambda(attr)
					;; map some attributes to numbers
					(cond [(member (car attr) '(x y z n degree))
					       (list (car attr) (x->number (cadr attr)))]
					      [else
					       attr]))
				      (cdr e)))))
	     ((sxpath '(crossing @)) crossings))))

  `(*TOP* (@ (status "OK"))
	  . ,(append (map (lambda(route)
			    `(routes (@ (summary "todo")
					(copyrights "todo"))
				     . ,(append
					 (map (lambda(e)
						`(Attributes
						  ;; todo: drop length?
						  (@ . ,(map (lambda(attr)
							       ;; format from/to/length numbers
							       (cond [(member (car attr) '(from to length))
								      (list (car attr) (x->number (cadr attr)))]
								     [else
								      attr]))
							     (cdr e)))))
					      ((sxpath '(linref linrefelem @)) route))
					 `((legs (@ ;; todo: conversion to seconds / not always available?!
						  (mentz-public-duration
						   ,(if-let1 r ((if-car-sxpath '(@ publicDuration *text*)) route)
							     r
							     ""))
						  )
						 ,(distance (apply +
								   (map (compose x->number partial-route-length)
									((sxpath '(itdPartialRoute)) route))))
						 ;; todo: bug?
						 ;; does not include switching/waiting times!
						 ;; => maybe use publicDuration route attribute!
						 ;; talk with jsz and glathoud
						 ;; note: publicDuration does not include
						 ;; time between requested dep/arr and returned dep/arr
						 ,(duration (apply +
								   (map partial-route-duration ((sxpath '(itdPartialRoute)) route))))
						 ,(start-location (partial-route-coords (first ((sxpath '(itdPartialRoute)) route))))
						 ,(end-location   (partial-route-coords (last ((sxpath '(itdPartialRoute)) route))))
						 . ,(map transform-pr
							 ((sxpath '(itdPartialRoute)) route))))
					 (transform-crossings
					  ((sxpath '(crossings)) route))
					 ((sxpath '(debug)) route))))
			  ((sxpath '(// itdRoute)) sxml))
		     ((sxpath '(debug)) sxml)
		     (transform-crossings ((sxpath '(crossings)) sxml)))))

(define google-style-debug (debug-dump google-style "/tmp/gstyle"))

(define (google-directions-v3-out jscallback jsfilters jsgeom thunk)

  (define (maybe-apply-jscallback json)
    (if (string-null? jscallback)
	json
	(list
	 jscallback
	 " && "
	 jscallback
	 "("
	 json
	 ")")))
  
  (define (js-error e google-code)
    ;; cause of client-side javascript troubles we have
    ;; to use http status code 200 for all errors :(
    `(,(cgi-header :content-type "application/javascript" :status 200)
      ,(maybe-apply-jscallback
	(to-json `(("status"  . ,google-code)
		   ("routes"  . #())
		   ("error" . ,(error->backtrace e))
		   ("error_class" . ,(x->string (class-name (class-of e))))
		   )))))
  
  (define (sxml->json sxml)
    (list (cgi-header :content-type "text/javascript")
	  (maybe-apply-jscallback
	   (to-json
	    (apply-filters
	     jsfilters
	     ;;(tee "/tmp/in.sxml"
	     (google-style sxml
                           ;; todo: provide alternatives
                           (let1 jsgeom (let1 r (if (not jsgeom)
                                                  'encoded
                                                  (read-from-string jsgeom))
                                          (if (list? r)
                                            r
                                            (list r)))
                             (debug-assert (list? jsgeom))
                             (debug-assert (symbol? (car jsgeom)))
                             (case (car jsgeom)
                               [(string)
                                (debug-assert (every exact? (cdr jsgeom)))
                                (let* ((permuter (if (null? (cdr jsgeom))
                                                   '(1 0 2)
                                                   (cdr jsgeom)))
                                       (fl (map (cut format-number-fixed-point <>)
                                                '(6 6 1 2))) ;; todo: allow to change via jsgeom? or use slib format? no not now
                                       (format-numbers (lambda(p)
                                                         (map-with-index
                                                          (lambda(idx f)
                                                            (f (ref p idx)))
                                                          fl)))
                                       (transform-point (compose (cut string-join <> ",")
                                                                 (cut permute <> permuter)
                                                                 format-numbers)))
                                  (lambda(xyzm)
                                    `(geom
                                      ,(string-join (map transform-point xyzm)
                                                    " "))))]
                               [(enconly)
                                (lambda(xyzm)
                                  `(polyline (@ (points ,(google-polyline-encode xyzm)))))]
                               [else
                                (lambda(xyzm)
                                  (let1 line-and-levels (google-polyline-encode-dp-inexact xyzm)
                                    `(polyline (@ (points ,(car line-and-levels))
                                                  (levels ,(cadr line-and-levels))))))]))))))))

  (guard (e
	  [(fatal-error? e)
	   (raise e)]  ;; todo: not really good
	  [(route-error? e)
	   (js-error e (case (route-error-code e)
			 [(time-limit-reached)   "ALPSTEIN_TIME_LIMIT_REACHED"]
			 [(vertex-limit-reached) "ALPSTEIN_VERTEX_LIMIT_REACHED"]
			 [(unreachable)          "ZERO_RESULTS"]
			 [(out-of-network)       "ALPSTEIN_OUT_OF_NETWORK"]
			 [else                   "UNKNOWN_ERROR"]))]
	  [else
	   (js-error e "UNKNOWN_ERROR")])
	 (sxml->json (thunk))))

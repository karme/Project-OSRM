#| -*- mode: scheme; coding: utf-8; -*- |#
;;; simple wrapper to map osrm API to our API
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

(define-module osrm-wrapper
  (use rfc.uri)
  (use rfc.http)
  (use www.cgi)
  (use util.list)
  (use gauche.sequence)
  (use srfi-14)
  (use rfc.json)
  (use sxml.adaptor) ;; for assert macro
  (use partial-route)
  (use google-directions)
  (use geod)
  (use www.fastcgi)
  (use routing)
  (use osrm-client)
  (export wrap-osrm-main))

(select-module osrm-wrapper)

(define (s->min x)
  (/. x 60))

(define (fake-4d pl)
  (map (lambda(x)
         (list (ref x 0) (ref x 1) 0 (ref x 2)))
       (geod-add-measure 'wgs84
                         (map (cut permute-to <list> <> '(1 0)) pl))))

(define (wrap-osrm-route-2 points)
  (let1 r (osrm-route points '())
    ;; todo: catch other errors!
    (when (and (= (assoc-ref r "status") 207)
               (equal? "Cannot find route between points" (assoc-ref r "status_message")))
      (error <route-error> :code 'unreachable))
    (let1 pr (make-partial-route
              (let1 geom (assoc-ref r "route_geometry")
                (assert (not (string? geom)))
                (fake-4d geom))
              (s->min (assoc-ref (assoc-ref r "route_summary") "total_time")))
      ;; compare osm distance vs our distance
      #?=(list (assoc-ref (assoc-ref r "route_summary") "total_distance") (partial-route-length pr))
      pr)))

;; todo: also in
(define (group-pairwise l)
  (assert (even? (size-of l)))
  (fold-right (lambda(n o) (if (or (null-list? o)
                                   (pair? (car o)))
                             (cons n o)
                             (cons (cons n (car o)) (cdr o))))
              (list)
              l))

(define (wrap-osrm-route params)
  (let ((jscallback (cgi-get-parameter "callback" params :default ""))
        (jsfilter (cgi-get-parameter "jsfilter" params :list #t :default '()))
        (jsgeom (cgi-get-parameter "jsgeom" params))
        (format (cgi-get-parameter "format" params :default "js"))
        (query (cgi-get-parameter "q" params)))
    (let ((render (assoc-ref `(("js"    . ,(cut google-directions-v3-out jscallback jsfilter jsgeom <>))
                               ("xml"   . ,render-xml)
                               ("sxml"  . ,render-sxml))
                             format)))
      (render (lambda()
                `(result
                  (itdRouteList
                   (itdRoute
                    ,(wrap-osrm-route-2 (group-pairwise (google-directions-query->track query)))))))))))

;; simple test
;; (wrap-osrm-route-2 '((8.983340995511963 . 48.52608311031189) (9.15725614289749 . 48.52975538424495)))
;; (wrap-osrm-route '(("q" "from:48.52608311031189,8.983340995511963to:48.52975538424495,9.15725614289749") ("format" "js")))

(define (create-context . args)
  (alist->hash-table '()))

(define (cgi-wrap-osrm context params)
  (wrap-osrm-route params))

(define (wrap-osrm-main config . args)
  #?=(list "started")
  ;; todo: do this in apache config
  (sys-putenv "PATH" "/usr/local/bin:/usr/bin:/bin")
  (let* ((context (create-context (config)))
         (handle-request (cut cgi-wrap-osrm context <>)))
    (with-fastcgi (cut cgi-main handle-request
		       :on-error (lambda(e) (raise e)) ;; overwrite default error handler and exit
		       )
                  :post-hook (ref context 'post-hook (lambda())))))

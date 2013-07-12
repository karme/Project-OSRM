#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; transform sxml to sql suitable for postgresql/postgis
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

(use sxml.adaptor) ;; for assert
(use sxml.sxpath)
(use sxml.tools)
(use util.list)
(use gauche.sequence)
(use srfi-1)
(use math.const)
(use text.tree)

(define way-id (car-sxpath '(@ id *text*)))
(define way-tags (compose (cute alist->hash-table <> 'equal?)
                          (cute map (lambda(tag) (cons (sxml:attr tag 'k) (sxml:attr tag 'v))) <>)
                          (sxpath '(tag))))

(define *profiles* '(foot bicycle mtb))

(define (ewkt-line srid . line)
  (assert (number? srid))
  (assert (>= (size-of line) 2))
  (string-append #`"SRID=,|srid|;LINESTRING("
                 (string-join (map (lambda(x)
                                     (string-join (map x->string x) " "))
                                   line)
                              ",")
                 ")"))

(define geographic->epsg3857
  (let* ((r 6378137.)
         (xs (/. (*. pi r) 180.)))
    (lambda(p)
      (assert (and (<= (cadr p) 90.0) (>= (cadr p) -90.0)))
      (list (*. (mod0 (car p) 360) xs)
            (*. (log (tan (*. (+. 90 (cadr p)) (/. pi 360))))
                r)))))

;; older gauche doesn't have generators
;; (define (once-generator first-value other-values) (gcons* first-value (circular-generator other-values)))
(define (once-generator first-value other-values)
  (let1 first #t
    (lambda() (cond [first
                     (set! first #f)
                     first-value]
                    [else
                     other-values]))))

(define (main args)
  (write-tree
   (list "create table costs ("
         (intersperse
          ",\n"
          (append (list "ogc_fid bigint primary key"
                        "osmid bigint not null"
                        "wkb_geometry geometry not null")
                  (append-map!
                   (lambda(p)
                     (list #`",|p|_costs_fwd double precision"
                           #`",|p|_costs_bwd double precision"
                           ;; todo: maybe remove again
                           #`",|p|_speed_fwd double precision"
                           #`",|p|_speed_bwd double precision"
                           ))
                   *profiles*)
                  (list "constraint enforce_dims_wkb_geometry check ((st_ndims(wkb_geometry) = 2))"
                        "constraint enforce_geotype_wkb_geometry check ((geometrytype(wkb_geometry) = 'LINESTRING'::text))"
                        "constraint enforce_srid_wkb_geometry check ((st_srid(wkb_geometry) = 3857))")))
         ");\n"))
  ;;(print "select addgeometrycolumn('public','costs','wkb_geometry',3857,'LINESTRING',2);")
  (print "insert into costs (ogc_fid, osmid, wkb_geometry, "
         (string-join (append-map!
                       (lambda(p)
                         (list #`",|p|_costs_fwd"
                               #`",|p|_costs_bwd"
                               #`",|p|_speed_fwd"
                               #`",|p|_speed_bwd"))
                       *profiles*)
                      ",")
         ") VALUES ")
  (let1 separator (once-generator '() ",\n")
    (until (read) eof-object? => expr
           (case (car expr)
             [(way)
              (let* ((id (way-id expr))
                     (tags (way-tags expr))
                     (geometry (string-split (ref tags "geometry") " "))
                     (osmid (ref tags "oldid" id)))
                (when (>= (size-of geometry) 2)
                  (write-tree
                   (list
                    (separator)
                    "("
                    id
                    ","
                    osmid
                    ",'"
                    (apply ewkt-line (cons 3857
                                           (map (lambda(p)
                                                  (let1 r (string-split p ",")
                                                    (geographic->epsg3857
                                                     (map string->number
                                                          (list (car r) (cadr r))))))
                                                geometry)))
                    "',"
                    (intersperse ","
                                 (append-map! (lambda(profile)
                                                ;; note: cost is a misnomer (costs really are: distance/cost)
                                                (list (ref tags #`"osrm:,|profile|:fwd:cost" "0")
                                                      (ref tags #`"osrm:,|profile|:bwd:cost" "0")
                                                      (ref tags #`"osrm:,|profile|:fwd:speed" "0")
                                                      (ref tags #`"osrm:,|profile|:bwd:speed" "0")
                                                      ))
                                              *profiles*))
                    ")"))))])))
  (print ";")
  (for-each print
            '("-- maybe you want to run:"
              "-- create index costs_geom_idx on costs using gist (wkb_geometry);"
              "-- vacuum analyze costs;"
              "-- some apps (f.e. qgis) need a 32-bit uniq id:"
              "-- alter table costs add column \"gid\" serial;"
              "-- create unique index costs_gid_idx on costs (gid);"))
  0)

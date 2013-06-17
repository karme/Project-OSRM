#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

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

(define *profiles* '(foot bicycle))

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

(define (main args)
  (write-tree
   (list "create table costs ("
         (intersperse ","
                      (append (list "ogc_fid bigint primary key"
                                    "wkb_geometry geometry")
                              (append-map!
                               (lambda(p)
                                 (list #`",|p|_costs_fwd double precision"
                                       #`",|p|_costs_bwd double precision"))
                               *profiles*)))
         ");"))
  ;;(print "select addgeometrycolumn('public','costs','wkb_geometry',3857,'LINESTRING',2);")
  (print "insert into costs (ogc_fid, wkb_geometry, "
         (string-join (append-map!
                       (lambda(p)
                         (list #`",|p|_costs_fwd"
                               #`",|p|_costs_bwd"))
                       *profiles*)
                      ",")
         ") VALUES ")
  (let1 first #t
    (until (read) eof-object? => expr
           (case (car expr)
             [(way)
              (let1 tags (way-tags expr)
                (write-tree
                 (list
                  (cond [first
                         (set! first #f)
                         ""]
                        [else
                         ","])
                  "("
                  (way-id expr)
                  ",'"
                  (let1 geometry (string-split (ref tags "geometry") " ")
                    (if (>= (size-of geometry) 2)
                      (apply ewkt-line (cons 3857
                                             (map (lambda(p)
                                                    (let1 r (string-split p ",")
                                                      (geographic->epsg3857
                                                       (map x->number
                                                            (list (car r) (cadr r))))))
                                                  geometry)))
                      ""))
                  "',"
                  (intersperse ","
                               (append-map! (lambda(profile)
                                              ;; todo: maybe use 'Infinity'::double precision instead of 0
                                              ;; but then fix the lua code first! (it already returns 0)
                                              (list (ref tags #`"osrm:,|profile|:fwd:cost" "0")
                                                    (ref tags #`"osrm:,|profile|:bwd:cost" "0")))
                                            *profiles*))
                  ")")))])))
  (print ";")
  0)

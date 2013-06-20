#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;; really split the ways (and do some other stuff):
;;; - drop unused nodes
;;; - apply way splits
;;; - profile ways
;;; - denormalize relations
;;; - fix way references in restriction relations
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

(use srfi-1)
(use util.list)
(use file.util)
(use huge-sparse-bitmap)
(use node-pos-map)
(use gauche.sequence)
(use sxml.adaptor) ;; for assert
(use sxml.sxpath)
(use sxml.tools)
(use dbm.gdbm)
(use srfi-19)
(use gauche.uvector)
(use binary.io)
(use elevation-profile)
(use lua)
(use gauche.process)

(define *profiles* '(foot bicycle mtb))

(define z (dem-stack->xy->z*))
(define upsample-polyline->4d (get-upsample-polyline->4d z))

(define-syntax info
  (syntax-rules ()
    ((_ ?form)
     (format/ss (current-error-port)
                "~a~a pid=~s INFO: ~a"
                (let1 si (debug-source-info '?form)
                  (or (and si
                           (string-append (car si)
                                          ":"
                                          (x->string (cadr si)) ": " ))
                      ""))
                (date->string (current-date) "~1 ~T") ;; .~N~z")
                (sys-getpid)
                ?form))))

(define-syntax warn
  (syntax-rules ()
    ((_ ?form)
     (format/ss (current-error-port)
                "~a~a pid=~s WARNING!: ~a"
                (let1 si (debug-source-info '?form)
                  (or (and si
                           (string-append (car si)
                                          ":"
                                          (x->string (cadr si)) ": " ))
                      ""))
                (date->string (current-date) "~1 ~T") ;; .~N~z")
                (sys-getpid)
                ?form))))

(define (string->exact x)
  (let1 n (string->number x)
    (assert n)
    (assert (exact? n))
    n))

(define (write-lines l)
  (for-each (lambda(x)
              (write x)
              (newline))
            l))

;; note: parallel-pipe expects complete packets and one for each input packet
(define (parallel-pipe-out-old x)
  (assert (list? x))
  (print (length x))
  (write-lines x)
  (flush))

(define (parallel-pipe-out x)
  (assert (list? x))
  (let1 blob (string->u8vector (with-output-to-string
                                 (lambda()
                                   (write-lines x))))
    (assert (< (size-of blob) (ash 1 25))) ;; 32MiB
    (write-u32 (size-of blob))
    (write-block blob)
    (flush)))

(define (node-ref x)
  `(nd (@ (ref ,(x->string x)))))

(define way-id (car-sxpath '(@ id *text*)))
(define way-nodes (compose (cute map string->exact <>) (sxpath '(nd @ ref *text*))))

(define way-without-nodes
  (let1 f (sxpath '((not@ nd)))
    (lambda(w)
      (cons (car w) (f w)))))

(define way-has-duration?
  (let1 f (if-car-sxpath "/tag[@k='duration']/@v/text()")
    (lambda(w)
      (boolean (f w)))))

(define (replace-id obj new-id)
  (let1 oldid (sxml:attr obj 'id)
    (assert oldid)
    (append
     (sxml:change-attr obj (list 'id (x->string new-id)))
     `((tag (@ (k "oldid") (v ,oldid)))))))

(define (at-least-two? x)
  (assert (list? x))
  (not (or (null? x)
           (null? (cdr x)))))

(define relation-references
  (let1 f (sxpath '(// @ ref *text*))
    (lambda(rel)
      (f rel))))

(define relation-type
  (let1 f (if-car-sxpath "/tag[@k='type']/@v/text()") ;; '(tag (@ (k (equal? "type"))) v *text*))
    (lambda(rel)
      (f rel))))

(define (is-restriction? rel)
  (if-let1 type (relation-type rel)
    (boolean (#/^restriction/ type))
    #f))

(define restriction-from
  (let1 f (if-car-sxpath "/member[@role='from']/@ref/text()")
    (lambda(rel)
      (f rel))))

(define restriction-from-type
  (let1 f (if-car-sxpath "/member[@role='from']/@type/text()")
    (lambda(rel)
      (f rel))))

(define restriction-to
  (let1 f (if-car-sxpath "/member[@role='to']/@ref/text()")
    (lambda(rel)
      (f rel))))

(define restriction-to-type
  (let1 f (if-car-sxpath "/member[@role='to']/@type/text()")
    (lambda(rel)
      (f rel))))

(define restriction-via
  (let1 f (if-car-sxpath "/member[@role='via']/@ref/text()")
    (lambda(rel)
      (f rel))))

(define restriction-via-type
  (let1 f (if-car-sxpath "/member[@role='via']/@type/text()")
    (lambda(rel)
      (f rel))))

(define restriction-replace-from!
  (let1 f (if-car-sxpath "/member[@role='from']/@ref")
    (lambda(rel n)
      (set-cdr! (f rel) (list (x->string n))))))

(define restriction-replace-to!
  (let1 f (if-car-sxpath "/member[@role='to']/@ref")
    (lambda(rel n)
      (set-cdr! (f rel) (list (x->string n))))))

(define split-new-id car)
(define split-nodes cdr)
(define (split-first-and-last-node s)
  (list (car (split-nodes s)) (last (split-nodes s))))

(define (debug-out x)
  (with-output-to-port (current-error-port)
    (lambda()
      (write x)
      (newline)
      (flush))))

(define node-id (compose string->exact (car-sxpath '(@ id *text*))))

(define (node-add-z expr z)
  (assert (number? z))
  ;;(sxml:add-attr expr (list 'z (number->string z)))
  (append expr `((tag (@ (k "z") (v ,(number->string z)))))))

;; todo: limit output precision
(define (polyline->string pl)
  (string-join (map (lambda(p)
                      (string-join (map number->string p) ","))
                    pl)
               " "))

(define (way-add-geometry expr pl-4d)
  (let ((way-length (number->string (last (last pl-4d)))))
    ;; (sxml:add-attr expr (list 'pl pl))
    (append expr `((tag (@ (k "length") (v ,way-length)))
                   ;; todo: maybe apply douglas-peucker
                   (tag (@ (k "geometry") (v ,(polyline->string pl-4d))))))))

(define way-tags (compose (cute alist->hash-table <> 'equal?)
                          (cute map (lambda(tag) (cons (sxml:attr tag 'k) (sxml:attr tag 'v))) <>)
                          (sxpath '(tag))))

(define (way-add-alpstein-waytype expr)
  ;; see also
  ;; http://wiki.openstreetmap.org/wiki/Key:surface

  ;; "For roads for motor vehicles there there is normally an
  ;; assumption that the surface is surface=paved"
  (define (motorway? highway)
    (boolean (member highway '("motorway"
                               "motorway_link"
                               "trunk"
                               "trunk_link"
                               "primary"
                               "primary_link"
                               "secondard"
                               "secondard_link"
                               "tertiary"
                               "tertiary_link"
                               "living_street"
                               "pedestrian"
                               "residiential"))))

  (let* ((tags (way-tags expr))
         (waytype (assoc-ref '(("asphalt"       . "A")
                               ("paved"         . "R")
                               ("unpaved"       . "G")
                               ("gravel"        . "S")
                               ("ground"        . "G")
                               ("dirt"          . "G")
                               ("grass"         . "G")
                               ("concrete"      . "W")
                               ("paving_stones" . "S")
                               ("cobblestone"   . "S")
                               ("sand"          . "S")
                               ("compacted"     . "P"))
                             (ref tags "surface"
                                  (and (motorway? (ref tags "highway" #f)) "paved"))
                             #f)))
    (if waytype
      (append expr
              `((tag (@ (k "alpstein:waytype")
                        (v ,waytype)))))
      expr)))

(define (main args)
  (let-optionals* (cdr args) ((output-format "parallel-pipe")
			      (way-splits "way-splits.dbm")
			      ;;(used-nodes "used-nodes.dbm")
                              (node-pos-file "node-pos.dbm")
                              (way-relation-file "way-relation.dbm")
                              (relation-file     "relation.dbm")
                              (cache-size "4096"))
    (sys-setenv "LUA_PATH" "../profiles/?.lua;;" #t)
    (let ((way-splits (dbm-open <gdbm> :path way-splits :rw-mode :read))
	  ;; (used-nodes (make-huge-sparse-bitmap used-nodes
          ;;                                      :cache-size (string->number cache-size)
          ;;                                      :rw-mode :read))
          (node-pos-map (node-pos-map-open node-pos-file :rw-mode :read))
          (way-relation (dbm-open <gdbm> :path way-relation-file :rw-mode :read))
          (relation     (dbm-open <gdbm> :path relation-file :rw-mode :read))
          (num-in-nodes 0)
          (dropped-nodes 0)
          (num-in-ways 0)
          (num-way-splits 0)
          (num-out-ways 0)
          (dropped-durations 0)
          (num-relations 0)
          (fixed-relations 0)
          (dropped-relations 0)
          (via-way-restrictions 0)
	  (failed-restrictions 0)
          (output-result (if (equal? output-format "parallel-pipe")
                           parallel-pipe-out
                           write-lines))
          (luas (map (lambda(profile)
                       (let* ((p (run-lua-repl :input :pipe :output :pipe))
                              (lua-luise (cute lua-eval-luise (process-output p) (process-input p) <...>))
                              (lua-file (cute string-append "../profiles/" <> ".lua")))
                         (lua-luise `(funcall dofile ,(lua-file (symbol->string profile))))
                         (assert (= (lua-luise '(funcall set_lowest_bit 2 1)) 3))
                         (cons profile lua-luise)))
                     *profiles*)))
      (let* ((node-pos-get (assoc-ref node-pos-map 'get)))
        (until (read) eof-object? => expr
               (output-result
                (case (car expr)
                  [(node)
                   (inc! num-in-nodes)
		   (cond [(node-pos-get (node-id expr) #f)
			  ;;(huge-sparse-bitmap-get-bit used-nodes (node-id expr))
			  (list
			   ;; (if (> (length pos) 2)
			   ;;   (node-add-z expr (caddr pos))
			   ;;   expr)
			   expr)]
			 [else
			  (inc! dropped-nodes)
			  (list)])]
                  [(way)
                   (let* ((profile-way (lambda(w)
                                         (let1 r (way-add-alpstein-waytype
                                                  (way-add-geometry w
                                                                    (upsample-polyline->4d 'wgs84
                                                                                           (map (lambda(n)
                                                                                                  (let1 p (node-pos-get n #f)
                                                                                                    ;; might fail if way references non-existing node
                                                                                                    (assert (and n (list? p)))
                                                                                                    p))
                                                                                                (way-nodes w))
                                                                                           50)))
                                           (append r
                                                   (append-map
                                                    (lambda(profile)
                                                      (append-map
                                                       (lambda(fwd/bwd l)
                                                         (assert (string? fwd/bwd))
                                                         (assert (list? l))
                                                         (map (lambda(x)
                                                                `(tag (@ (k ,#`"osrm:,|profile|:,|fwd/bwd|:,(car x)")
                                                                         (v ,(x->string (cdr x))))))
                                                              l))
                                                       '("fwd" "bwd")
                                                       (receive l ((assoc-ref luas profile)
                                                                   `(funcall way_info
                                                                             (funcall fakeway
                                                                                      ,(way-id r)
                                                                                      ,(map (lambda(x)
                                                                                              (cons (string->symbol (sxml:attr x 'k)) (sxml:attr x 'v))) ((sxpath '(tag)) r)))))
                                                         (assert (list? l))
                                                         (assert (= (size-of l) 2))
                                                         (assert (list? (car l)))
                                                         (assert (list? (cadr l)))
                                                         l)))
                                                    (map car luas))))))
                          (split-way (lambda(l)
                                       (assert (list? l))
                                       (inc! num-way-splits)
                                       (inc! num-out-ways (length l))
                                       (map profile-way l)))
                          (copy-way (lambda(w)
                                      (inc! num-out-ways)
                                      (list (profile-way w)))))
                     (inc! num-in-ways)
                     ;; denormalize relations
                     (for-each-with-index (lambda(idx rid-and-role)
                                            (append! expr
                                                     (let1 rel (read-from-string (dbm-get relation (number->string (assoc-ref rid-and-role 'id))))
                                                       (cons `(tag (@ (k ,#`"rel[,|idx|]:role")
                                                                      (v ,(assoc-ref rid-and-role 'role))))
                                                             (map (lambda(t)
                                                                    (assert (eq? (car t) 'tag))
                                                                    (let1 oldkey (sxml:attr t 'k)
                                                                      (sxml:change-attr t (list 'k #`"rel[,|idx|][,|oldkey|]"))))
                                                                  ((sxpath '(tag)) rel))))))
                                          (read-from-string (dbm-get way-relation (way-id expr) "()")))
                     
                     (let1 splits (read-from-string (dbm-get way-splits (sxml:attr expr 'id) "#f"))
                       (cond [splits
                              ;; have to split that way
                              (when (way-has-duration? expr)
                                ;; todo:
                                ;; spliting durations is not supported for now
                                ;; how could one support them?
                                ;; best solution i can think of at the moment:
                                ;; split duration depending on length :(
                                ;; for now we drop durations on split
                                (inc! dropped-durations)
                                (set! expr (way-remove-duration expr)))
                              (let ((w (way-without-nodes expr)))
                                (split-way (map (lambda(s)
                                                  (append (replace-id w (split-new-id s))
                                                          (map node-ref (split-nodes s))))
                                                splits)))]
                             [else
                              (copy-way expr)])))]
                  [(relation)
                   (let ((drop-relation  (lambda(r)
                                           (inc! dropped-relations)
                                           '()))
                         (copy-relation  list)
                         (fixed-relation (lambda(r)
                                           (inc! fixed-relations)
                                           (list r))))
                     (inc! num-relations)
                     (cond [(any (cute dbm-get way-splits <> #f)
                                 (relation-references expr))
                            ;; this relation must be fixed!
                            ;; todo:
                            (cond [(and (is-restriction? expr)
                                        (and-let* ((from (restriction-from expr))
                                                   (via  (restriction-via expr)) ;; todo: there are restrictions without via?!
                                                   (to   (restriction-to expr)))
                                          (guard (e [else
                                                     (debug-out `((expr . ,expr)
                                                                  (from . ,from)
                                                                  (via  . ,via)
                                                                  (to   . ,to)
                                                                  (refs . ,(relation-references expr))
                                                                  (splits . ,(map (cute dbm-get way-splits <> #f) (relation-references expr)))))
                                                     (inc! failed-restrictions)
                                                     (drop-relation expr)])
                                                 (let ((from-split (read-from-string (dbm-get way-splits from "#f")))
                                                       (to-split   (read-from-string (dbm-get way-splits to "#f")))
                                                       (via (string->exact via)))
                                                   (assert (or from-split to-split))
                                                   (cond [(equal? (restriction-via-type expr) "node")
                                                          (when from-split
                                                            (assert (equal? (restriction-from-type expr) "way"))
                                                            (assert (find (lambda(s) (member via (split-first-and-last-node s)))
                                                                          from-split))
                                                            (restriction-replace-from!
                                                             expr
                                                             (split-new-id (find (lambda(s) (member via (split-first-and-last-node s)))
                                                                                 from-split))))
                                                          (when to-split
                                                            (assert (equal? (restriction-to-type expr) "way"))
                                                            (assert (find (lambda(s) (member via (split-first-and-last-node s)))
                                                                          to-split))
                                                            (restriction-replace-to!
                                                             expr
                                                             (split-new-id (find (lambda(s) (member via (split-first-and-last-node s)))
                                                                                 to-split))))
                                                          (fixed-relation expr)]
                                                         [(equal? (restriction-via-type expr) "way")
                                                          ;; todo: via may be ways
                                                          ;; s.a. http://wiki.openstreetmap.org/wiki/Relation:restriction
                                                          ;; does osrm support via ways?
                                                          ;; seems so? but only one node is stored?!
                                                          (inc! via-way-restrictions)
                                                          (drop-relation expr)]
                                                         [else
                                                          (assert #f)])))))]
                                  [else
                                   ;; todo: drop for now
                                   ;; #?=`(todo drop relation of type ,(relation-type expr))
                                   (drop-relation expr)])]
                           [else
                            ;; copy relation
                            (copy-relation expr)]))]
                  [else
                   ;; copy
                   (list expr)]))))
      (for-each dbm-close (list way-splits way-relation relation))
      (node-pos-map-close node-pos-map)
      
      (info (format #f "dropped ~s/~s nodes\n" dropped-nodes num-in-nodes))
      (info (format #f "splited ~s/~s ways into ~s ways\n" num-way-splits num-in-ways num-out-ways))
      (when (> dropped-durations 0)
        (warn (format #f "dropped ~s duration tag(s)\n" dropped-durations)))
      
      (info (format #f "fixed ~s/~s relations\n" fixed-relations num-relations))
      (when (> dropped-relations 0)
        (warn (format #f "dropped ~s relations\n" dropped-relations)))
      (when (> failed-restrictions 0)
        (warn (format #f "dropped ~s restrictions because of error\n" failed-restrictions)))
      (when (> via-way-restrictions 0)
        (warn (format #f "dropped ~s via-way restrictions\n" via-way-restrictions)))))
  0)

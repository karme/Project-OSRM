#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
;;;
;;;parse and store osm relations in dbm files
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

(use util.list) ;; for assoc-ref
(use sxml.adaptor) ;; for assert
(use sxml.sxpath)
(use sxml.tools)
(use dbm.gdbm)
(use srfi-1)

(define (string->exact x)
  (let1 n (string->number x)
    (assert n)
    (assert (exact? n))
    n))

(define relation-id
  (compose string->exact (car-sxpath '(@ id *text*))))

;; todo: return role
(define relation-references
  (let1 f (sxpath '(member @ ref *text*))
    (lambda(rel)
      (f rel))))

(define relation-references-and-roles
  (let ((members (sxpath '(member)))
        (ref-and-role (sxpath '(@ (or@ ref role)))))
    (let1 f (compose (cute map ref-and-role <>)
                     members)
      (lambda(rel)
        (f rel)))))

(define relation-type
  ;; '(tag (@ (k (equal? "type"))) v *text*))
  (let1 f (if-car-sxpath "/tag[@k='type']/@v/text()")
    (lambda(rel)
      (f rel))))

(define (is-route? rel)
  (if-let1 type (relation-type rel)
    (boolean (#/^route/ type))
    #f))

(define relation-without-members
  (let1 f (sxpath '((not@ member)))
    (lambda(r)
      (cons (car r) (f r)))))

(define (main args)
  (let-optionals* (cdr args) ((way-relation-file "way-relation.dbm")
                              (relation-file     "relation.dbm"))
    (let ((way-relation (dbm-open <gdbm> :path way-relation-file))
          (relation     (dbm-open <gdbm> :path relation-file)))
      (until (read) eof-object? => expr
             (assert (eq? (car expr) 'relation))
             ;; filter interesting relations
             ;; todo: maybe there are other interesting relation types?
             ;; note: in my osrm lua profile i assume at the moment,
             ;; that every relation has a type!
             (when (is-route? expr)
               (let1 id (relation-id expr)
                 (for-each (lambda(ref-and-role)
                             (let1 ref (car (assoc-ref ref-and-role 'ref))
                               (dbm-put! way-relation
                                         ref
                                         (write-to-string (cons `((id . ,id)
                                                                  (role . ,(car (assoc-ref ref-and-role 'role))))
                                                                (read-from-string (dbm-get way-relation ref "()")))))))
                           (relation-references-and-roles expr))
                 (dbm-put! relation
                           (number->string id)
                           (write-to-string (relation-without-members expr))))))
      (dbm-close relation)
      (dbm-close way-relation)))
  0)

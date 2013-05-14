#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

(use dbm.gdbm)
(use sxml.adaptor) ;; for assert
(use sxml.sxpath)
(use sxml.tools)

(define way-id (car-sxpath '(@ id *text*)))
(define way-tags (compose (cute alist->hash-table <> 'equal?)
                          (cute map (lambda(tag) (cons (sxml:attr tag 'k) (sxml:attr tag 'v))) <>)
                          (sxpath '(tag))))
  
(define (main args)
  (let-optionals* (cdr args) ((db-file "ways.dbm"))
    (let ((db (dbm-open <gdbm> :path db-file :rw-mode :write)))
      (until (read) eof-object? => expr
             (case (car expr)
               [(way)
                (let1 tags (way-tags expr)
                  (dbm-put! db (way-id expr)
                            (write-to-string `((geom . ,(map (lambda(p)
                                                               (map string->number (string-split p ",")))
                                                             (string-split (ref tags "geometry") " ")))
                                               (speed . ,(map (lambda(profile)
                                                                (cons profile
                                                                      (cons (string->number (ref tags #`"osrm:,|profile|:fwd:speed" "0"))
                                                                            (string->number (ref tags #`"osrm:,|profile|:bwd:speed" "0")))))
                                                              '(foot bicycle)))))))]))))
  0)


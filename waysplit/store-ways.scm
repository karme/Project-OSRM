#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

(use dbm.gdbm)
(use sxml.adaptor) ;; for assert
(use sxml.sxpath)

(define way-id (car-sxpath '(@ id *text*)))
(define way-geometry (car-sxpath "/tag[@k='geometry']/@v/text()"))
  
(define (main args)
  (let-optionals* (cdr args) ((db-file "ways.dbm"))
    (let ((db (dbm-open <gdbm> :path db-file :rw-mode :write)))
      (until (read) eof-object? => expr
             (case (car expr)
               [(way)
                (dbm-put! db (way-id expr) (way-geometry expr))]))))
  0)

#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -Ftokyocabinet -I. -- $0 "$@"

(use dbm)
(cond-expand
 (tokyocabinet
  (use dbm.tokyocabinet)
  (define *dbclass* <tcbdb>))
 (else
  (use dbm.gdbm)
  (define *dbclass* <gdbm>)))

(use sxml.adaptor) ;; for assert
(use sxml.sxpath)
(use sxml.tools)
(use util.list)

(define way-id (car-sxpath '(@ id *text*)))
(define way-tags (compose (cute alist->hash-table <> 'equal?)
                          (cute map (lambda(tag) (cons (sxml:attr tag 'k) (sxml:attr tag 'v))) <>)
                          (sxpath '(tag))))

(define *profiles* '(foot bicycle mtb))

(define (main args)
  (let-optionals* (cdr args) ((db-file "ways.dbm"))
    (let ((db (dbm-open *dbclass* :path db-file :rw-mode :write)))
      (unwind-protect
       (until (read) eof-object? => expr
              (case (car expr)
                [(way)
                 (let1 tags (way-tags expr)
                   (dbm-put! db (way-id expr)
                             (write-to-string `((geom . ,(map (lambda(p)
                                                                (map string->number (string-split p ",")))
                                                              (string-split (ref tags "geometry") " ")))
                                                (waytype . ,(ref tags "alpstein:waytype" "U"))
                                                (speed . ,(map (lambda(profile)
                                                                 (cons profile
                                                                       (cons (string->number (ref tags #`"osrm:,|profile|:fwd:speed" "0"))
                                                                             (string->number (ref tags #`"osrm:,|profile|:bwd:speed" "0")))))
                                                               *profiles*))))))]))
       (dbm-close db))))
  0)


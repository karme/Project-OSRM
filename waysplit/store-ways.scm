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
(define way-tags
  (compose (cute alist->hash-table <> 'equal?)
           (cute map (lambda(tag) (cons (sxml:attr tag 'k) (sxml:attr tag 'v))) <>)
           (sxpath '(tag))))

(define *profiles* '(foot bicycle mtb))

;; todo: ugly: carry must be mixed in here at the moment :(
(define (alpstein-waytype profile tags)
  (let (;; see also bicycle.lua mode_pushing
        (carryfwd? (logbit? 2 (string->number (ref tags #`"osrm:,|profile|:fwd:mode" "0"))))
        (carrybwd? (logbit? 2 (string->number (ref tags #`"osrm:,|profile|:bwd:mode" "0"))))
        (waytype (assoc-ref '(("A" . "1")
                              ("S" . "4")
                              ("W" . "2")
                              ("P" . "3")
                              ("G" . "5")
                              ("R" . "7"))
                            (ref tags "alpstein:waytype" "U")
                            "0")))
    (cons (if carryfwd? "6" waytype)
          (if carrybwd? "6" waytype))))

(define (transform-way expr)
  (let1 tags (way-tags expr)
    (write-to-string
     `((geom . ,(map (lambda(p)
                       (map string->number (string-split p ",")))
                     (string-split (ref tags "geometry") " ")))
       (profile . ,(map (lambda(profile)
                          (cons profile
                                `((waytype . ,(alpstein-waytype profile tags))
                                  (speed . ,(cons (string->number (ref tags #`"osrm:,|profile|:fwd:speed" "0"))
                                                  (string->number (ref tags #`"osrm:,|profile|:bwd:speed" "0")))))))
                        *profiles*))))))

(define (main args)
  (let-optionals* (cdr args) ((db-file "ways.dbm"))
    (let ((db (dbm-open *dbclass* :path db-file :rw-mode :write)))
      (unwind-protect
       (until (read) eof-object? => expr
              (case (car expr)
                [(way)
                 (dbm-put! db (way-id expr) (transform-way expr))]))
       (dbm-close db))))
  0)


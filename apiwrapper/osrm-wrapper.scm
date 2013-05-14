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
  (use srfi-1)
  (use srfi-14)
  (use rfc.json)
  (use sxml.adaptor) ;; for assert macro
  (use partial-route)
  (use google-directions)
  (use elevation-profile-client)
  (use www.fastcgi)
  (use routing)
  (use osrm-client)
  (use dbm.gdbm)
  (export wrap-osrm-main))

(select-module osrm-wrapper)

(define (s->min x)
  (/. x 60))

;; todo: i already have a better implementation somewhere?!
(define (reverse-polyline-4d pl)
  (if (< (size-of pl) 2)
    pl
    (reverse (fold2 (lambda(n1 n2 o1 o2)
                      (values (cons (append n1 (list (+ o2 n2))) o1)
                              (+ o2 n2)))
                    '()
                    0
                    (fold (lambda(n o) (cons (permute n '(0 1 2)) o)) '() pl)
                    (cons 0 (fold2 (lambda(n o1 o2) (values (cons (- (ref n 3) o2) o1) (ref n 3))) '() 0 pl))))))

;; (reverse-4d '((x1 y1 z1 0) (x2 y2 z2 1) (x3 y3 z3 3) (x4 y4 z4 100)))

;; todo: i already have a better implementation somewhere?!
(define (merge-polyline-4d-2 a b)
  (cond [(null? a)
         b]
        [(null? b)
         a]
        [else
         (assert (zero? (last (car a))))
         (assert (zero? (last (car b))))
         (assert (equal? (subseq (last a) 0 3) (subseq (car b) 0 3)))
         (append a
                 (let1 l (last (last a))
                   (adjust-polyline-4d-offset b l)))])) ;; todo: cdr b?!

;; (let ((b '((9.0567383 48.5198042 423.1379008383598 0.0) (9.0568301 48.5197476 423.12746733354504 9.252167970969891) (9.0571689 48.5196919 423.1433522297799 35.035282356543675) (9.057403063097688 48.519664611762614 423.1587945691838 52.59770999429127)))
;;       (a '((9.057039407261255 48.51971318909548 423.13728086402386 0.0) (9.0571689 48.5196919 423.1433522297799 9.85456344503482) (9.0577773 48.519621 423.18347434716765 55.485070900249205) (9.0577773 48.519621 423.18347434716765 55.485070900249205))))
;;   #?=(last a)
;;   #?=(car b)
;;   (merge-polyline-4d-2 a b))

(define (merge-polyline-4d . l)
  (fold (lambda(n o)
          (merge-polyline-4d-2 o n))
        '()
        l))

(define (vec-lerp a b x)
  (map + a (map (cute * <> x) (map - b a))))

(define (lerp a b x)
  (+ a (* (- b a) x)))

(define (linref pl segment ratio)
  ;; find segment in polyline
  ;; note: there are precision troubles
  ;; => try to duplicate osrm's handling of coordinates and use best match
  (receive(from to)
      (let ((pl (map (lambda(p)
                       (map (compose floor->exact (cute * <> 100000)) (subseq p 0 2)))
                     pl)))
        (apply values
               (map (lambda(sp)
                      (caar
                       (sort
                        (map-with-index (lambda(idx p)
                                          (cons idx (apply max (map (compose abs -) p sp))))
                                        pl)
                        (lambda(a b)
                          (< (cdr a) (cdr b))))))
                    (map (lambda(p)
                           (map (compose floor->exact (cute * <> 100000)) p))
                         segment))))
    (assert (<= (abs (- from to)) 4))
    (let1 r (/. (lerp (last (ref pl from)) (last (ref pl to)) ratio)
                (last (last pl)))
      (assert (>= r 0))
      (assert (<= r 1))
      r)))

;; (linref '((9.0567383 48.5198042 423.1379008383598 0) (9.0568301 48.5197476 423.12746733354504 9.252167970969891) (9.0571689 48.5196919 423.1433522297799 35.035282356543675) (9.0577773 48.519621 423.18347434716765 80.66578981175806)) '((9.05716 48.51969) (9.05777 48.51961)) '0.49701599099977706)

;; bug:
;; (linref '((9.1658091 48.5268179 410.7279722525904 0.0) (9.166340674927433 48.52703568854599 410.06422471507847 46.13166450804794) (9.166872254413441 48.52725347462921 409.8843005640924 92.26332901639063) (9.167403838458037 48.52747125824961 409.1228009002175 138.39499352395973) (9.167935427061257 48.52768903940716 408.40584715174396 184.5266580325159) (9.168467020223122 48.527906818101805 408.0421412681178 230.65832254028487) (9.168998617943636 48.528124594333526 407.76564472066235 276.78998704796606) (9.169530220222853 48.528342368102265 406.9708384232763 322.92165155590567) (9.170061827060765 48.52856013940799 406.42012056471793 369.05331606409834) (9.17059343845742 48.528777908250674 406.07952716148725 415.1849805725343) (9.171125054412828 48.52899567463026 404.89010916161243 461.31664508050096) (9.17165667492701 48.52921343854671 402.91011833192357 507.4483095886973) #0=(9.1721883 48.5294312 402.15331554088874 553.5799740978238) #0#) '((9.1658 48.52681) (9.17218 48.52943)) 0.6070125419499107)

;; (define polyline->grass-line
;;           (lambda(xyzm)
;;               (print "L " (size-of xyzm))
;;               (for-each (lambda(p)
;;                           (print (string-join (map x->string
;;                                                    (append
;;                                                     (subseq (oproject (ref p 0) (ref p 1) 0) 0 2)
;;                                                     (list (ref p 2))))
;;                                               " ")))
;;                         xyzm)))

(define (osrm-geometry r)
  (let1 geom (assoc-ref r "route_geometry")
    (assert (vector? geom))
    (map (cut permute-to <list> <> '(1 0)) geom)))

(define (start-segment r)
  (map (lambda(p) (list (ref p 1) (ref p 0)))
       (coerce-to <list> (subseq (assoc-ref r "phantomsegments") 0 2))))

(define (end-segment r)
  (map (lambda(p) (list (ref p 1) (ref p 0)))
       (coerce-to <list> (subseq (assoc-ref r "phantomsegments") 2 4))))

(define (calculate-start r)
  (apply vec-lerp (append (start-segment r)
                          (list (ref (assoc-ref r "phantomratios") 0)))))

(define (calculate-end r)
  (apply vec-lerp (append (end-segment r)
                          (list (ref (assoc-ref r "phantomratios") 1)))))

(define (start-point r)
  (let1 p (ref (assoc-ref r "via_points") 0)
    (list (ref p 1) (ref p 0))))

(define (end-point r)
  (assert (= (size-of (assoc-ref r "via_points")) 2))
  (let1 p (ref (assoc-ref r "via_points") 1)
    (list (ref p 1) (ref p 0))))

(define (start-way-id r)
  (assoc-ref (assoc-ref r "route_summary") "start_point"))

(define (end-way-id r)
  (assoc-ref (assoc-ref r "route_summary") "end_point"))

(define (uniq l)
  (if (null? l)
    l
    (reverse (fold (lambda(n o)
                     (if (equal? n (car o))
                       o
                       (cons n o)))
                   (list (car l))
                   (cdr l)))))

(define one? (cute = 1 <>))

(define (range start stop)
  (if (< start stop)
    (cons start (range (+ start 1) stop))
    '()))

;; todo: crap
(define (polyline-4d-substring-2 pl from to)
  (assert (<= from to))
  (assert (>= from 0))
  (assert (<= to 1))
  (assert (zero? (last (car pl))))
  (let ((from (* from (last (last pl))))
        (to   (* to (last (last pl))))
        (find-interval (lambda(x)
                         (- (or (find-with-index (lambda(t) (> t x)) (map last pl))
                                (size-of pl))
                            1))))
    (let ((beg (find-interval from))
          (end (find-interval to)))
      (let ((lerp-interval (lambda(i t)
                             (if (>= i (- (size-of pl) 1))
                               (last pl)
                               (vec-lerp (ref pl i) (ref pl (+ i 1))
                                         (/ (- t (ref* pl i 3))
                                            (- (ref* pl (+ i 1) 3) (ref* pl i 3))))))))
        (cons (lerp-interval beg from)
              (append (map (cute ref pl <>) (range (+ beg 1) (+ end 1)))
                      (list (lerp-interval end to))))))))

(define (polyline-4d-length pl)
  (assert (zero? (last (car pl))))
  (last (last pl)))

(define (adjust-polyline-4d-offset pl . args)
  (let-optionals* args ((o (- (last (car pl)))))
    (if (zero? o)
      pl
      (map (lambda(p)
             (let1 p (reverse p)
               (reverse (cons (+ (car p) o) (cdr p)))))
           pl))))

(define (polyline-4d-substring pl from to)
  (adjust-polyline-4d-offset
   (if (<= from to)
     (polyline-4d-substring-2 pl from to)
     (polyline-4d-substring-2 (reverse-polyline-4d pl) (- 1 from) (- 1 to)))))

;; (polyline-4d-substring '((0 0 0 0) (50 0 0 50) (100 0 0 100)) 0.2 0.1)
;; (polyline-4d-substring '((0 0 0 0) (50 0 0 50) (100 0 0 100)) 1 0.4)

(define (wrap-osrm-route-2 context points)
  ;; todo: maybe cache
  (define (way-info id) (read-from-string (dbm-get (ref context 'db) id)))

  (define (way-geometry-2 id)
    (assoc-ref (way-info id) 'geom))

  (define (way-geometry id start end)
    (polyline-4d-substring (way-geometry-2 id) start end))

  (define (way-speed id start end)
    (/ ((if (<= start end)
          car
          cdr)
        #?=(assoc-ref (way-info id) 'speed))
       3.6))

  ;; (with-output-to-file "/tmp/lastcall2"
  ;;   (lambda()
  ;;     (write `(wrap-osrm-route-2 ',context ',points))))
  
  (let1 r (osrm-route points '() (ref context 'osrm-service '("localhost:5000" "/viaroute")))
    ;; todo: catch other errors!
    (when (and (= (assoc-ref r "status") 207)
               (equal? "Cannot find route between points" (assoc-ref r "status_message")))
      (error <route-error> :code 'unreachable))
    ;; (write r)
    ;; (newline)
    (let* ((way-list (uniq (map (lambda(x)
                                  (cons (ref x 0)
                                        (if (odd? (ref x 1)) ;; forward or backward?
                                          '(0 1)
                                          '(1 0))))
                                (assoc-ref r "segments"))))
           (start-linref (list (start-way-id r)
                               (linref (way-geometry (start-way-id r) 0 1)
                                       (start-segment r)
                                       (ref (assoc-ref r "phantomratios") 0))))
           (end-linref (list (end-way-id r)
                             (linref (way-geometry (end-way-id r) 0 1)
                                     (end-segment r)
                                     (ref (assoc-ref r "phantomratios") 1)))))
      (assert (< (apply max (append (map (compose abs -) (calculate-start r) (start-point r))
                                    (map (compose abs -) (calculate-end r)   (end-point r))))
                 2e-5))
      (assert (not (null? way-list)))
      (let* ((way-list (cond [(= (size-of way-list) 1)
                              (assert (equal? (ref start-linref 0) (ref end-linref 0)))
                              (list (append start-linref (list (ref end-linref 1))))]
                             [else
                              (assert (equal? (ref start-linref 0) (ref* way-list 0 0)))
                              (assert (equal? (car (last way-list)) (ref end-linref 0)))
                              (cons (append start-linref (list (ref* way-list 0 2)))
                                    (append (drop-right* (cdr way-list) 1)
                                            (list (list (ref end-linref 0)
                                                        (ref (last way-list) 1)
                                                        (ref end-linref 1)))))]))
             (ways (map (lambda(l) (apply way-geometry l)) #?=way-list))
             (speeds (map (lambda(l) (apply way-speed l)) way-list))
             (total-time (apply + (map (lambda(s v) (/ s v))
                                       (map polyline-4d-length ways)
                                       speeds)))
             (geometry (apply merge-polyline-4d ways)))
        ;; (write geometry)
        ;; (newline)
        (let1 pr (make-partial-route
                  ;;(upsample-polyline->4d (ref context 'elpro '("localhost" "/cgi-bin/elevation-profile.fcgi")) (osrm-geometry r) 50)
                  geometry
                  (s->min total-time))
          ;; compare our time vs osrm time (osrm time is wrong because it uses cost function not speed function)
          #?=(list total-time (assoc-ref (assoc-ref r "route_summary") "total_time"))
          ;; compare osm distance vs our distance
          ;; #?=(list (assoc-ref (assoc-ref r "route_summary") "total_distance") (partial-route-length pr))
          (append pr `((waylist . ,(map (lambda(w)
                                          `(way (@ (id ,(car w))
                                                   (from ,(number->string (cadr w)))
                                                   (to ,(number->string (caddr w))))))
                                        way-list)))))))))

;; some test
;; (wrap-osrm-route-2 (create-context '()) '((9.056513613489894 . 48.520302993378365) (9.06195045150861 . 48.51884638248705)))
;; only one way test
;; (wrap-osrm-route-2 (create-context '()) '((9.057477104737538 . 48.51966197480457) (9.057076304500677 . 48.51970535679522)))

;; ;; same segment
;; (wrap-osrm-route-2 (create-context '()) '((9.056919947513997 . 48.51972690702891) (9.056966664085719 . 48.51972328537291)))
;; ;; segment neighbour
;; (wrap-osrm-route-2 (create-context '()) '((9.056919947513997 . 48.51972690702891) (9.057409424005773 . 48.519660990468545)))
;; ;; segment between start/end
;; (wrap-osrm-route-2 (create-context '()) '((9.056788641422633 . 48.51977465264033) (9.057409424005773 . 48.519660990468545)))
;; ;; neighbours on different ways
;; (wrap-osrm-route-2 (create-context '()) '((9.057611179130467 . 48.519640198026586) (9.05789010911645 . 48.51960587189234)))

;; todo: bug?!
;; (wrap-osrm-route-2 (create-context '((osrm-service . ("localhost:5001" "/viaroute")))) '((9.133412948421148 . 48.532286470455524) (9.169766836495409 . 48.528323387733835)))

;; todo: also in
(define (group-pairwise l)
  (assert (even? (size-of l)))
  (fold-right (lambda(n o) (if (or (null-list? o)
                                   (pair? (car o)))
                             (cons n o)
                             (cons (cons n (car o)) (cdr o))))
              (list)
              l))

(define (wrap-osrm-route context params)
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
                    ,(wrap-osrm-route-2 context (group-pairwise (google-directions-query->track query)))))))))))

;; simple test
;; (wrap-osrm-route-2 '((8.983340995511963 . 48.52608311031189) (9.15725614289749 . 48.52975538424495)))
;; (wrap-osrm-route '(("q" "from:48.52608311031189,8.983340995511963to:48.52975538424495,9.15725614289749") ("format" "js")))

(define (create-context al)
  (alist->hash-table (acons 'db (dbm-open <gdbm> :path "../waysplit/ways.dbm" :rw-mode :read)
                            al)))

(define (wrap-osrm-main config . args)
  (when (eq? (port-buffering (current-error-port)) :none)
    (set! (port-buffering (current-error-port)) :line))
  #?=(list "started")
  ;; todo: do this in apache config
  (sys-putenv "PATH" "/usr/local/bin:/usr/bin:/bin")
  (let* ((context (create-context (config)))
         (handle-request (cut wrap-osrm-route context <>)))
    (with-fastcgi (cut cgi-main handle-request
		       :on-error (lambda(e) (raise e)) ;; overwrite default error handler and exit
		       )
                  :post-hook (ref context 'post-hook (lambda())))))

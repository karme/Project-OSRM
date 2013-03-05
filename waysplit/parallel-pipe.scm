#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*-
#GC_PRINT_STATS=1
#export GC_PRINT_STATS
exec gosh -I. -- "$0" "$@"
|#
;;;
;;;tool to process a stream in parallel
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
(use gauche.process)
(use gauche.sequence)
(use gauche.threads)
(use sxml.adaptor) ;; for assert
(use gauche.uvector)
(use binary.io)
(use pipe)

(define (adjust-pipe-buffer-size p)
  ;;#?=(get-pipe-buffer-size p)
  (set-pipe-buffer-size! p (ash 1 20))
  ;;#?=(get-pipe-buffer-size p)
  )

(define (launch-jobs jobs cmd-spec)
  (let1 r (map (lambda _ (run-process cmd-spec
                                      :input :pipe
                                      :output :pipe))
               (iota jobs))
    (for-each (lambda(p)
                (adjust-pipe-buffer-size (process-input p))
                (adjust-pipe-buffer-size (process-output p)))
              r)
    r))

(define (muxer p mux muxfwd)
  (let ((jobs (size-of p))
        (nip 0)
        (muxed 0))
    ;; don't want to handle incomplete reads
    (for-each (lambda(x) (set! (port-buffering x) :full))
              p)
    (while (any (compose not port-closed?) p)
      (assert (not (port-closed? (ref p nip))))
      (with-input-from-port (ref p nip)
        (lambda()
          ;; NOTE:
          ;; mux might block (no output from worker ready)
          ;; if the worker writes one packet for each packet received
          ;; this is not permanent and there is no dead-lock
          ;; otherwise:
          ;; => we don't read from the other workers
          ;; => they might block in write (pipe buffer full)
          ;; => our writer thread might hang in write to one of those workers
          ;; (unfortunately i didn't find a good solution for
          ;; non-blocking writes in gauche without using native code)
          ;; => the worker we are waiting for doesn't get new data
          ;; => it blocks and produces no output
          ;; => dead lock
          
          ;; a solution would be to do non-blocking reads via select:
          ;; handle input using gauche.selector reading all? available
          ;; packets and then forward them in order
          
          ;; but i didn't like my first try on that and decided it is
          ;; better to require the worker to write one packet for each
          ;; packet received
          (let1 r (mux)
            (cond [(eof-object? r)
                   (close-input-port (current-input-port))]
                  [else
                   (muxfwd r)
                   (inc! muxed)]))))
      (set! nip (modulo (+ nip 1) jobs)))
    muxed))

(define (demuxer p demux demuxfwd)
  (let ((jobs (size-of p))
        (nop 0)
        (demuxed 0))
    (port-for-each (lambda(x)
                     (with-output-to-port (ref p nop)
                       (lambda()
                         (demuxfwd x)))
                     (set! nop (modulo (+ nop 1) jobs))
                     (inc! demuxed))
                   demux)
    (for-each (cut close-input-port <>) p)
    demuxed))

;; todo
(define (getf x)
  (eval x (interaction-environment)))

;; todo: better rename to print&flush ?!
(define (print-line l)
  (print l)
  (flush))

(define (write-lines l)
  (for-each (lambda(x) (write x) (newline)) l))

(define (print-lines l)
  (for-each print l))

(define (read-lines)
  (let1 s (read-line)
    (if (eof-object? s)
      s
      (map (lambda _ (read-line))
           (iota (string->number s))))))

(define read-blob
  (let1 buf (make-u8vector (ash 1 20))
    (lambda()
      (let1 s (read-u32)
        (cond [(eof-object? s)
               s]
              [(> s 0)
               (when (> s (size-of buf))
                 #?=s
                 (set! buf (make-u8vector s)))
               (let1 got (read-block! buf (current-input-port) 0 s)
                 (assert (not (eof-object? got)))
                 (assert (= got s))
                 (list buf s))]
              [else
               (list buf s)])))))

(define (write-blob r)
  (receive (buf s) (apply values r)
    (when (> s 0)
      (write-block buf (current-output-port) 0 s))))

(define (main args)
  (adjust-pipe-buffer-size (current-output-port))
  (receive (jobs demux demuxfwd mux muxfwd . rest)
      (apply values (cdr args))
    (let* ((p (launch-jobs (x->number jobs) rest))
           (muxer-thread (make-thread (cut muxer
                                           (map process-output p)
                                           (getf (string->symbol mux))
                                           (getf (string->symbol muxfwd)))))
           (muxer-wait (lambda()
                         (guard (e [(<uncaught-exception> e)
                                    (error "muxer died with error" (ref e 'reason))])
                                (thread-join! muxer-thread)))))
      (thread-start! muxer-thread)
      (let ((demuxed (demuxer (map process-input p)
                              (let1 f (getf (string->symbol demux))
                                (lambda()
                                  (when (not (eq? (thread-state muxer-thread) 'runnable))
                                    (muxer-wait))
                                  (f)))
                              (getf (string->symbol demuxfwd))))
            (muxed (muxer-wait)))
        (assert (= demuxed muxed)))))
  0)

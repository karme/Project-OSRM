(define-module mmap
  (use runtime-compile)
  (use file.util)
  (use c-wrapper)
  ;; (use gauche.process)
  (export
   mmap-cast))

(select-module mmap)

(define-macro (debug-assert e)
  `(if (not ,e)
       (error "Debug assertion failed: " ,(x->string e))))

(c-load '("stdlib.h" "sys/mman.h" "fcntl.h" "sys/types.h" "unistd.h" "stdint.h")
        :import '(mmap madvise mlock msync munmap open close O_RDWR O_CREAT PROT_READ
                       PROT_WRITE MAP_PRIVATE MAP_SHARED MS_SYNC
		       MADV_NORMAL MADV_RANDOM MADV_SEQUENTIAL MADV_WILLNEED MADV_DONTNEED
		       POSIX_MADV_NORMAL POSIX_MADV_RANDOM POSIX_MADV_SEQUENTIAL POSIX_MADV_WILLNEED POSIX_MADV_DONTNEED
                       int8_t uint8_t int16_t uint16_t int32_t uint32_t int64_t uint64_t))

(define (file-create fname)
  (open-output-file fname :if-does-not-exist :create :element-type :binary)
  #t)

;; make sure file is at least fsize bytes
(define (file-resize fname fsize)
  (when (not (file-exists? fname))
    (file-create fname))
  (when (< (file-size fname) fsize)
    (let ((port (open-output-file fname :element-type :binary :if-exists :overwrite)))
      ;;       (port-seek port 0 SEEK_END)
      ;;       #?=fsize
      ;;       #?=(port-tell port)
      ;;       #?=(< (port-tell port) fsize)
      ;;       #?=(port-seek port 0 SEEK_SET)
      #?=(port-seek port (- fsize 1))
      (write-byte 0 port)
      (flush port)))
  (debug-assert (<= fsize #?=(file-size fname))))

(define (mmap-readonly fname size)
  (let* ((port (open-input-file fname))
         (filedes (port-file-number port))
	 (r (mmap 0 size PROT_READ MAP_SHARED filedes 0)))
    ;; (time #?=(madvise r size POSIX_MADV_WILLNEED))
    ;; (time #?=(mlock r size))
    r))

(define (mmap-writable fname size)
  (file-resize fname size)
  ;; note: using open directly because there is no standard way to
  ;; open read/write?
  (let* ((filedes (open fname (logior O_RDWR O_CREAT)))
         (ret (mmap 0
		    size
		    (logior PROT_READ PROT_WRITE)
		    MAP_SHARED
		    filedes
		    0)))
    (debug-assert (not (= -1 filedes)))
    ;; closing file does not close memory maping
    (close filedes)
    ;;(time #?=(madvise ret size POSIX_MADV_WILLNEED))
    ;;(time #?=(mlock ret size))
    ret))

;; (define (hd fname)
;;   (run-process `(hd ,fname) :wait #t))

(define (mmap-c-type fname c-type writable)
  (let ((csize (c-sizeof c-type)))
    (mmap-cast fname csize writable (lambda(x)
                                      (cast (ptr c-type) x)))))

(define (register-finalizer-2! objptr final)
  (register-finalizer! objptr final)
  (values objptr
          (cute final objptr)))

(define (mmap-sync addr bytes)
  (debug-assert (zero? (msync addr bytes MS_SYNC))))

(define (mmap-cast fname bytes writable mmcast)
  (register-finalizer-2! (mmcast
                          ((if writable mmap-writable mmap-readonly)
			   fname bytes))
                         (lambda(x)
                           #?=(format #f "mmap finalizer called for ~s\n" x)
                           (mmap-sync x bytes)
                           (munmap x bytes) ;; todo: maybe check for zero
                           ;; (hd fname)
                           )))

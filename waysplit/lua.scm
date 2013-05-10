;;;
;;; simple lua interface
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
(define-module lua
  (use gauche.process)
  (use srfi-13)
  (use text.tree)
  (use util.list)
  (export run-lua-repl
          lua-eval-string
          lua-eval-luise
          stop-lua-repl))

(select-module lua)

;; launch a lua repl
(define (run-lua-repl . args)
  (apply run-process (cons '(lua -e "
function sexp(x)
   if type(x)=='table' then
     io.write('(');
     for k,v in pairs(x) do
       io.write('(');
       sexp(k);
       io.write(' . ');
       sexp(v);
       io.write(')');
     end;
     io.write(')');
   elseif type(x)=='boolean' and x then
     io.write('#t');
   elseif type(x)=='boolean' and (not x) then
     io.write('#f');
   elseif type(x)=='number' then
     io.write(tostring(x));
   else
     io.write((string.gsub(string.format('%q',tostring(x)),'\\\\\\n','\\\\n')));
   end
end

function repl_print(...)
   sexp({...});
   io.write('\\n');
   io.flush();
end;
for line in io.lines() do
   repl_print(pcall(loadstring(\"return \"..line)));
end
")
                           args)))

(define (luise x)
  (cond [(and (list? x) (not (null? x)) (symbol? (car x)) (eq? (car x) 'funcall) (not (null? (cdr x))))
         ;; hmm maybe bad idea / function call
         (append (list (symbol->string (cadr x)) #\()
                 (intersperse #\,
                              (map luise (cddr x)))
                 (list ")"))]
        [(and (list? x)
              (every pair? x))
         ;; render as table (unfortunately needs ugly hack)
         (append (list "(function() local t={};")
                 (map (lambda(kv)
                        (list "t["
                              (luise (x->string (car kv)))
                              "]="
                              (luise (cdr kv))
                              ";"))
                      x)
                 (list "return t; end)()"))]
        [(and (pair? x) (not (null? x)) (symbol? (car x)))
         (append (list #\{ (luise (car x)) #\=)
                 (list (luise (cdr x)))
                 (list #\}))]
        [(or (list? x) (vector? x))
         (append (list #\{)
                 (intersperse #\, (map luise x))
                 (list #\}))]
        [else
         (write-to-string x)]))

(define (lua-eval-tree in out t)
  (with-ports in out #f
              (lambda()
                ;;#?=(tree->string t)
                (write-tree t)
                (newline)
                (flush)
                (let* ((line (read-line))
                       (r (read-from-string line)))
                  (if (cdar r)
                    (apply values (map cdr (cdr r)))
                    ;; todo: maybe use specific condition
                    (error "lua error" (cdadr r)))))))

(define lua-eval-string lua-eval-tree)

(define (lua-eval-luise in out . args)
  (lua-eval-tree in out (apply luise args)))

(define (stop-lua-repl p)
  (close-output-port (process-input p))
  (process-wait p #f #t))

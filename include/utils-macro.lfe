; Here we create all redis-cmd-* macros
; redis-cmd-* macros are created by macro redis-cmd

(defmacro redis-cmd (small-type return-decoder)
 `(progn ,@(list
  `(defsyntax ,(mk-a 'redis-cmd small-type)
    ([command-name]                       (redis-cmd-mk command-name () ,return-decoder))
    ([command-name command-args]          (redis-cmd-mk command-name command-args ,return-decoder))
    ([fun-name command-name command-args] (redis-cmd-mk fun-name command-name command-args ,return-decoder)))

   `(defsyntax ,(mk-a (mk-a 'redis-cmd small-type) '-fixed)
     ([command-name]                         (redis-cmd-mk-fixed command-name () ,return-decoder))
     ([command-name command-args]            (redis-cmd-mk-fixed command-name command-args ,return-decoder))
     ([fun-name command-name command-args]   (redis-cmd-mk-fixed fun-name command-name command-args ,return-decoder)))

   `(defsyntax ,(mk-a (mk-a 'redis-cmd small-type) '-skip)
     ([n command-name command-args]   (redis-cmd-mk-skip n command-name command-args ,return-decoder))))))

; newcmd is for quickly adding commands for full coverage.  Their return values haven't
; been checked for their "erlangy-ness" and you will probably get {ok, Val} tuples back
; instead of stripped ok values.
; Redis defines commands with either fixed positive arity (>= 0) or a negative
; minimal, but unlimited, arity (-3 means "takes three or more arguments")
(defmacro newcmd
 ([name argc] (when (>= argc 0)) `(redis-cmd-mk-fixed ,name ,(la (xn argc)) default-return))
 ([name argc]                    `(redis-cmd-mk ,name ,(la (xn (abs argc))) default-return)))

(redis-cmd -n     redis-return-nil)
(redis-cmd -s     redis-return-status)
(redis-cmd -i     redis-return-integer)
(redis-cmd -f     redis-return-float)
(redis-cmd -l     redis-return-single-line)
(redis-cmd -b     redis-return-bulk)
(redis-cmd -m     redis-return-multibulk)
(redis-cmd -m-pl  redis-return-multibulk-pl)
(redis-cmd -m-kl  redis-return-multibulk-kl)
(redis-cmd -strip redis-return-strip-ok)
(redis-cmd -o     redis-return-special)
(redis-cmd -i-tf  redis-return-integer-true-false)

;  Here we make all er_key/{1..N} functions
(defmacro make-key-generator-of-max-args (len)
 (let* ((arg-names (: lists map (fun xn 1) (: lists seq 1 len)))
        (fns (: lists map (fun mk-key-fun 1) arg-names)))
  `(progn ,@fns)))

(defmacro return-type (name redis-cmds)
  `(defun ,(mk-a-return-type name) () ',redis-cmds))

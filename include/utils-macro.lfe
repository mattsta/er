; Here we create all redis-cmd-* macros
; redis-cmd-* macros are created by macro redis-cmd

(defmacro redis-cmd (small-type return-decoder)
 `(defsyntax ,(mk-a 'redis-cmd small-type)
   ([command-name]                       (redis-cmd-mk command-name () ,return-decoder))
   ([command-name command-args]          (redis-cmd-mk command-name command-args ,return-decoder))
   ([fun-name command-name command-args] (redis-cmd-mk fun-name command-name command-args ,return-decoder))))

(redis-cmd -n     redis-return-nil)
(redis-cmd -s     redis-return-status)
(redis-cmd -i     redis-return-integer)
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

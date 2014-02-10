(defmodule (erp client)
  (export all))

(eval-when-compile 
  (include-file "include/utils.lfe"))

(include-file "include/utils-macro.lfe")

(defmacro redis-cmd-mk
  ((command-name command-args wrapper-fun-name)
    (let* ((cmd (b command-name)))
     `(defun ,command-name (,@command-args)
        (,wrapper-fun-name (: er_redis q client (list ,cmd ,@command-args))))))
  ((fun-name command-name command-args wrapper-fun-name)
    (let* ((cmd (b command-name)))
     `(defun ,fun-name (,@command-args)
        (,wrapper-fun-name (: er_redis q client (list ,cmd ,@command-args)))))))

(include-file "include/redis-return-types.lfe")
(include-file "include/redis-cmds.lfe")

(defun er_next ()
  (redis-return-strip-ok (: gen_server call client 'next 'infinity)))

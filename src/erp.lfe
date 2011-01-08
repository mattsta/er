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

(defun er_transaction ((txn-fun) (when (is_function txn-fun 1))
 (let ((safe-redis-exec
        (lambda (client-for-txn)
         (funcall txn-fun client-for-txn)
         (try
          (: er exec client-for-txn)
          (catch
           ; EXEC without MULTI means we had a discard command in the txn-fun
           ((tuple 'throw (tuple 'redis_error #b("ERR EXEC without MULTI")) o)
            'discarded)
           ((tuple 'throw Throwed o) (throw Throwed)))))))
 (case (: er multi client)
  ((tuple use-cxn 'ok) (funcall safe-redis-exec use-cxn))
  ('ok (funcall safe-redis-exec client))))))

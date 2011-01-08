(defmodule er
  (export all))

(eval-when-compile 
  (include-file "include/utils.lfe"))

(include-file "include/utils-macro.lfe")

(defmacro redis-cmd-mk
  ((command-name command-args wrapper-fun-name)
    (let* ((cmd (b command-name)))
     `(defun ,command-name (client ,@command-args)
        (,wrapper-fun-name (: er_redis q client (list ,cmd ,@command-args))))))
  ((fun-name command-name command-args wrapper-fun-name)
    (let* ((cmd (b command-name)))
     `(defun ,fun-name (client ,@command-args)
        (,wrapper-fun-name (: er_redis q client (list ,cmd ,@command-args)))))))

(include-file "include/redis-return-types.lfe")
(include-file "include/redis-cmds.lfe")

(defun er_next (client)
  (redis-return-strip-ok (: gen_server call client 'next 'infinity)))

; if client is an er_pool, you get a private PID so other processes using
;  the pool won't disturb your sequential operations.
; if the client is an er_redis, you operate normally.
; er_pool returns a private pid on multi and cleans it up on exec
(defun er_transaction ((client txn-fun) (when (is_function txn-fun 1))
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

(defmodule eru
 (export all))

(eval-when-compile
 (include-file "include/utils.lfe"))

(include-file "include/utils.lfe")
(include-file "include/utils-macro.lfe")

(defun dump_all (server)
 (dump server #b("*")))

(defun dump (server pattern)
 (: lists map
  (lambda (k) (tuple k (value server k)))
  (: er keys server pattern)))
;  (lc ((<- k (: er keys server pattern))) (tuple k (value server k))))

(defun value (server key)
 (value server key (: er type server key)))

(defun value
 ((server key 'string) (: er get server key))
 ((server key 'list)   (: er lrange server key 0 'inf))
 ((server key 'set)    (: er smembers server key))
 ((server key 'zset)   (: er zrevrange server key 0 (: er zcard server key)))
 ((server key 'hash)   (: er hgetall_k server key)))

(make-key-generator-of-max-args 32)

(defun hcopy (server from to)
 (: er hmset server to (: er hgetall server from)))

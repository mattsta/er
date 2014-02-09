;; These functions wrap default Redis return values
;; to return more erlang-y return values.
;; (example: return atom "ok" instead of binary OK;
;;           return tuples/proplists/maps instead of
;;           just lists with items paired mod 2.

(defun redis-return-nil (x) x)

(defun redis-return-status 
  ([(tuple 'error bin)] (throw (tuple 'redis_error bin)))
  ([x] (when (is_binary x))
    (list_to_atom (: string to_lower (binary_to_list x))))
  ([#b("QUEUED")] 'queued)
  ([(list x)] (when (is_binary x))
    ; we trust redis to have a stable list of return atoms
    (list_to_atom (: string to_lower (binary_to_list x))))
  ([(tuple pid status)] (when (is_pid pid)) (tuple pid (redis-return-status status))))

(defun redis-return-integer 
  ([(list #b("inf"))] 'inf)
  ([(list #b("-inf"))] '-inf)
  ([(list #b("nan"))] 'nan)
  ([x] (when (is_integer x)) x)
  ([(tuple 'ok x)] (when (is_integer x)) x)
  ([(tuple 'ok 'nil)] 'nil)
  ([(tuple 'ok #b("inf"))] 'inf)
  ([(tuple 'ok #b("-inf"))] '-inf)
  ([(tuple 'ok #b("nan"))] 'nan)
  ([(tuple 'ok x)] (when (is_binary x)) (list_to_integer (binary_to_list x)))
  ([#b("QUEUED")] 'queued)
  ([(list x)] (when (is_binary x)) (list_to_integer (binary_to_list x)))
  ([(tuple 'error bin)] (throw (tuple 'redis_error bin))))

(defun redis-return-single-line
  ([()] #b())
  ([(tuple 'ok value)] value)
  ([#b("QUEUED")] 'queued)
  ([(list x)] x))

(defun redis-return-bulk
  ([(cons (tuple 'ok value) xs)] (cons value (redis-return-bulk xs)))
  ([(tuple 'ok value)] value)
  ([#b("QUEUED")] 'queued)
  ([x] x))

(defun to-proplist
  ([()] '())
  ([(cons a (cons b xs))] (cons (tuple (binary_to_atom a 'utf8) b) (to-proplist xs))))

(defun to-keylist
  ([()] '())
  ([(cons a (cons b xs))] (cons (tuple a b) (to-keylist xs))))

(defun redis-return-multibulk-pl (x)
  (to-proplist (redis-return-multibulk x)))

(defun redis-return-multibulk-kl (x)
  (to-keylist (redis-return-multibulk x)))

(defun redis-return-multibulk 
  ([(tuple 'ok 'nil)] 'nil)
  ([x] (when (is_atom x)) x)
  ([x] (when (is_list x)) (element 2 (: lists unzip x)))
  ([#b("QUEUED")] 'queued))

(defun redis-return-strip-ok
  ([()] ())
  ([(tuple pid retval)] (when (is_pid pid)) (tuple pid (redis-return-strip-ok retval)))
  ([(cons (tuple 'ok #b("message")) xs)] (cons 'message (redis-return-strip-ok xs)))
  ([(cons (tuple 'ok #b("subscribe")) xs)] (cons 'subscribe (redis-return-strip-ok xs)))
  ([(cons (tuple 'ok value) xs)] (cons value (redis-return-strip-ok xs)))
  ([(cons x xs)] (cons x (redis-return-strip-ok xs)))
  ([#b("QUEUED")] 'queued))

(defun redis-return-special
  ([potential-errors-or-ignorable-return-values]
   (when (is_list potential-errors-or-ignorable-return-values))
   (: lists map
    (match-lambda
     ([(tuple 'error bin)] (throw (tuple 'redis_error bin)))
     ([x] x))
    potential-errors-or-ignorable-return-values))
  ([(tuple 'error bin)] (throw (tuple 'redis_error bin)))
  ([x] x))

;; Functions for handling more specialized return types
(defun redis-return-integer-true-false
    ([0] 'false)          ; er_redis converts some things to ints
    ([(list #b("0"))] 'false)  ; and others it leaves in binaries
    ([1] 'true)
    ([(list #b("1"))] 'true)
    ([#b("QUEUED")] 'queued))

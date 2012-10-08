;; include/redis-return-types.lfe
;; What is this file?  Redis return type conversion functions.
;; What are these (return-type * ...) sexps?
;;   - I don't remember exactly.  It was going to be a way to auto-generate
;;     something that didn't pan out.  The list hasn't been kept in sync with
;;     redis-cmds.lfe.  They can probably all be deleted.
;; Under these return-type declarations are the actual return conversion functions.
;; The goal is to return as much actual information as possible so clients don't
;;   have to needlessly unpack tuples that just say {ok, Value} or other nonsense.

;; create list of all return types available as (return-type::return-types)
(return-type return-types
  (nil status integer single-line bulk multibulk special))

;; create lists of functions with each return type
;; available as (return-type::nil), (return-type::status), etc
(return-type nil
  (quit))

(return-type status
  (auth type rename select flushdb flushall set
   setex mset nset rpush lpush ltrim lset sinterstore
   sunionstore sdiffstore hmset save bgsave shutdown
   bgrewriteaof slaveof))

(return-type integer
  (exists del renamenx dbsize expire expireat ttl
   move setnx msetnx incr incrby decr decrby append
   llen lrem sadd srem smove scard sismember zadd
   zrem zincrby zcard zremrangebyrank zremrangebyscore
   zunionstore zinterstore hset hincrby hexists hdel hlen
   publish lastsave))

(return-type bulk
  (keys get getset substr lindex lpop rpop rpoplpush 
   spop srandmember zrank zrevrank zscore hget info ))

(return-type single-line
  (randomkey))

(return-type multibulk
  (mget lrange blpop brpop sinter sunion sdiff smembers
   zrange zrevrange zrangebyscore hkeys hvals hgetall
   sort multi exec discard))

(return-type special
  (subscribe unsubscribe psubscribe punsubscribe monitor))


;; Functions for handling generic return types
(defun redis-return-nil (x) x)

(defun redis-return-status 
  ([(tuple 'error bin)] (throw (tuple 'redis_error bin)))
  ([x] (when (is_binary x))
    (list_to_atom (: string to_lower (binary_to_list x))))
  ([#b("QUEUED")] 'queued)
  ([(x)] (when (is_binary x))
    ; we trust redis to have a stable list of return atoms
    (list_to_atom (: string to_lower (binary_to_list x))))
  ([(tuple pid status)] (when (is_pid pid)) (tuple pid (redis-return-status status))))

(defun redis-return-integer 
  ([(#b("inf"))] 'inf)
  ([(#b("-inf"))] '-inf)
  ([(#b("nan"))] 'nan)
  ([x] (when (is_integer x)) x)
  ([(tuple 'ok x)] (when (is_integer x)) x)
  ([(tuple 'ok 'nil)] 'nil)
  ([(tuple 'ok #b("inf"))] 'inf)
  ([(tuple 'ok #b("-inf"))] '-inf)
  ([(tuple 'ok #b("nan"))] 'nan)
  ([(tuple 'ok x)] (when (is_binary x)) (list_to_integer (binary_to_list x)))
  ([#b("QUEUED")] 'queued)
  ([(x)] (when (is_binary x)) (list_to_integer (binary_to_list x)))
  ([(tuple 'error bin)] (throw (tuple 'redis_error bin))))

(defun redis-return-single-line
  ([()] #b())
  ([(tuple 'ok value)] value)
  ([#b("QUEUED")] 'queued)
  ([(x)] x))

(defun redis-return-bulk
  ([((tuple 'ok value) . xs)] (cons value (redis-return-bulk xs)))
  ([(tuple 'ok value)] value)
  ([#b("QUEUED")] 'queued)
  ([x] x))

(defun to-proplist
  ([()] '())
  ([(a b . xs)] (cons (tuple (binary_to_atom a 'utf8) b) (to-proplist xs))))

(defun to-keylist
  ([()] '())
  ([(a b . xs)] (cons (tuple a b) (to-keylist xs))))

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
  ([((tuple 'ok #b("message")) . xs)] (cons 'message (redis-return-strip-ok xs)))
  ([((tuple 'ok #b("subscribe")) . xs)] (cons 'subscribe (redis-return-strip-ok xs)))
  ([((tuple 'ok value) . xs)] (cons value (redis-return-strip-ok xs)))
  ([(x . xs)] (cons x (redis-return-strip-ok xs)))
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
    ([(#b("0"))] 'false)  ; and others it leaves in binaries
    ([1] 'true)
    ([(#b("1"))] 'true)
    ([#b("QUEUED")] 'queued))

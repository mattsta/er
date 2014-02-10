;; include/redis-cmds.lfe
;; What is this?  All redis commands and then some.
;; This file gets directly included by src/er.lfe and src/erp.lfe
;; Return macros come from include/utils-macro.lfe which must be included
;;   before this file in src/er.lfe and src/erp.lfe.

;; cmd macros define the return value for native erlang conversion
;; redis-cmd-n = nil return
;; redis-cmd-s = status return
;; redis-cmd-i = int return
;; redis-cmd-l = line return
;; redis-cmd-b = bulk return
;; redis-cmd-m = multibulk return
;; redis-cmd-m-pl = return a property list [{key, <<"value">>}, ..] (auto-atomize keys)
;; redis-cmd-m-kl = return a key list [{<<"key">>, <<"value">>}, ...]
;; redis-cmd-strip = convert things like {ok, Value} to Value
;; redis-cmd-o = other/special return
;; redis-cmd-i-tf = int return where 0 = false and 1 = true

;; Connection handling ;;

; close the connection
(redis-cmd-n quit)

; simple password authentication if enabled
(redis-cmd-s-fixed auth (_pass_))

;; Connection commands ;;

; ping the server
(redis-cmd-s-fixed ping)

; echo the given string
(redis-cmd-b-fixed echo (_msg_))

;; Commands operating on all the kind of values ;;

; test if a key exists
; returns true on exists; false on not exists
(redis-cmd-i-tf-fixed exists (_key_))

; delete a key
; returns number of keys deleted: 0 to N
(redis-cmd-i del (_keys_))

; return the type of the value stored at key
(redis-cmd-s-fixed type (_key_))

; return all the keys matching a given pattern
(redis-cmd-b-fixed keys (_pattern_))

; return a random key from the key space
(redis-cmd-l-fixed randomkey)

; rename the old key in the new one, destroing the newname key if it already exists
; > er:rename(C, bob2, bob3).
; ** exception throw: {redis_error,<<"ERR no such key">>}
; > er:rename(C, bob3, bob3).
; ** exception throw: {redis_error,<<"ERR source and destination objects are the same">>}
(redis-cmd-s-fixed rename (_oldname_ _newname_))

; rename the old key in the new one, if the newname key does not already exist
(redis-cmd-i-tf-fixed renamenx (_oldname_ _newname_))

; return the number of keys in the current db
(redis-cmd-i-fixed dbsize)

; set a time to live in seconds on a key
(redis-cmd-i-tf-fixed expire (_key_ _seconds-forward_))
(redis-cmd-i-tf-fixed expireat (_key_ _unixtime_))

; remove a previously set expire
(redis-cmd-i-tf-fixed persist (_key_))

; get the time to live in seconds of a key
(redis-cmd-i-fixed ttl (_key_))

; Select the DB having the specified index
(redis-cmd-s-fixed select (_index_))

; Move the key from the currently selected DB to the DB having as index dbindex
(redis-cmd-i-fixed move (_key_ _dbindex_))

; Remove all the keys of the currently selected DB
(redis-cmd-s-fixed flushdb)

; Remove all the keys from all the databases
(redis-cmd-s-fixed flushall)


;; Commands operating on string values ;;

; set a key to a string value
(redis-cmd-s set (_key_ _value_))

; return the string value of the key
(redis-cmd-b-fixed get (_key_))

; set a key to a string returning the old value of the key
(redis-cmd-b-fixed getset (_key_ _value_))

; multi-get, return the strings values of the keys
; _key1_ _key2_ ... _keyN_
(redis-cmd-m mget (_keys_))

; set a key to a string value if the key does not exist
(redis-cmd-i-tf-fixed setnx (_key_ _value_))

; Set+Expire combo command
(redis-cmd-s-fixed setex (_key_ _time_ _value_))

; set a multiple keys to multiple values in a single atomic operation
; _key1_ _value1_ _key2_ _value2_ ... _keyN_ _valueN_
(redis-cmd-s mset (_key-value-pairs_))

; set a multiple keys to multiple values in a single atomic operation if none of the keys already exist
; _key1_ _value1_ _key2_ _value2_ ... _keyN_ _valueN_
(redis-cmd-i-tf msetnx (_key-value-pairs_))

; increment the integer value of key
(redis-cmd-i-fixed incr (_key_))

; increment the integer value of key by integer
(redis-cmd-i-fixed incrby (_key_ _integer_))

; decrement the integer value of key
(redis-cmd-i-fixed decr (_key_))

; decrement the integer value of key by integer
(redis-cmd-i-fixed decrby (_key_ _integer_))

; append the specified string to the string stored at key
(redis-cmd-i-fixed append (_key_ _value_))

; return a substring out of a larger string
(redis-cmd-b-fixed substr (_key_ _start_ _end_))   ; substr = getrange in 2.2+
(redis-cmd-b-fixed getrange (_key_ _start_ _end_)) ; getrange = substr for redis 2.2+

(redis-cmd-i-fixed setrange (_key_ _start_ _end_))

(redis-cmd-i-fixed getbit (_key_ _position_))
(redis-cmd-i-fixed setbit (_key_ _position_ _value_))

; return the length of a string
(redis-cmd-i-fixed strlen (_key_))

;; Commands operating on lists ;;

; Append an element to the tail of the List value at key
(redis-cmd-i rpush (_key_ _value_))

; Append an element to the head of the List value at key
(redis-cmd-i lpush (_key_ _value_))

; Return the length of the List value at key
(redis-cmd-i-fixed llen (_key_))

; Return a range of elements from the List at key
(redis-cmd-m-fixed lrange (_key_ _start_ _end_))

; Trim the list at key to the specified range of elements
(redis-cmd-s-fixed ltrim (_key_ _start_ _end_))

; Return the element at index position from the List at key
(redis-cmd-b-fixed lindex (_key_ _index_))

; Push on the left if the list exists.
; Returns number of elements in list.  Returns zero if list isn't created.
(redis-cmd-i-fixed lpushx (_key_ _value_))

; Push on the right if the list exists.
; Returns number of elements in list.  Returns zero if list isn't created.
(redis-cmd-i-fixed rpushx (_key_ _value_))

; Insert before or after a value in a list
; Returns the number of elements in list
(redis-cmd-i-fixed linsert (_key_ _before_or_after_ _existing_value_ _new_value_))

; Set a new value as the element at index position of the List at key
(redis-cmd-s-fixed lset (_key_ _index_ _value_))

; Remove the first-N, last-N, or all the elements matching value from the List at key
(redis-cmd-i-fixed lrem (_key_ _count_ _value_))

; Return and remove (atomically) the first element of the List at key
(redis-cmd-b-fixed lpop (_key_))

; Return and remove (atomically) the last element of the List at key
(redis-cmd-b-fixed rpop (_key_))

; Blocking LPOP
; _key1_ _key2_ ... _keyN_ _timeout_
(redis-cmd-m blpop (_keys_ _timeout_))

; Blocking RPOP
; _key1_ _key2_ ... _keyN_ _timeout_
(redis-cmd-m brpop (_keys_ _timeout_))

; Return and remove (atomically) the last element of the source List stored at _srckey_ and push the same element to the destination List stored at _dstkey_
(redis-cmd-b-fixed rpoplpush (_srckey_ _dstkey_))

; Blocking rpoplpush
(redis-cmd-b-fixed brpoplpush (_srckey_ _dstkey_))



;; Commands operating on sets ;;

; Add the specified member to the Set value at key
(redis-cmd-i-tf sadd (_key_ _member_))

; Remove the specified member from the Set value at key
(redis-cmd-i-tf srem (_key_ _member_))

; Remove and return (pop) a random element from the Set value at key
(redis-cmd-b-fixed spop (_key_))

; Move the specified member from one Set to another atomically
(redis-cmd-i-tf-fixed smove (_srckey_ _dstkey_ _member_))

; Return the number of elements (the cardinality) of the Set at key
(redis-cmd-i-fixed scard (_key_))

; Test if the specified value is a member of the Set at key
(redis-cmd-i-tf-fixed sismember (_key_ _member_))

; Return the intersection between the Sets stored at key1, key2, ..., keyN
; _key1_ _key2_ ... _keyN_
(redis-cmd-m sinter (_keys_))

; Compute the intersection between the Sets stored at key1, key2, ..., keyN, and store the resulting Set at dstkey
; _dstkey_ _key1_ _key2_ ... _keyN_
(redis-cmd-s sinterstore (_dstkey_ _keys_))

; Return the union between the Sets stored at key1, key2, ..., keyN
; _key1_ _key2_ ... _keyN_
(redis-cmd-m sunion (_keys_))

; Compute the union between the Sets stored at key1, key2, ..., keyN, and store the resulting Set at dstkey
; _dstkey_ _key1_ _key2_ ... _keyN_
(redis-cmd-s sunionstore (_dstkey_ _keys_))

; Return the difference between the Set stored at key1 and all the Sets key2, ..., keyN
; _key1_ _key2_ ... _keyN_
(redis-cmd-m sdiff (_keys_))

; Compute the difference between the Set key1 and all the Sets key2, ..., keyN, and store the resulting Set at dstkey
; _dstkey_ _key1_ _key2_ ... _keyN_
(redis-cmd-s sdiffstore (_dstkey_ _keys_))

; Return all the members of the Set value at key
(redis-cmd-m-fixed smembers (_key_))

; Return a random member of the Set value at key
(redis-cmd-b srandmember (_key_))


;; Commands operating on sorted sets (zsets, Redis version >= 1.1) ;;

; Add the specified member to the Sorted Set value at key or update the score if it already exist
(redis-cmd-i zadd (_key_ _score_ _member_))

; Remove the specified member from the Sorted Set value at key
(redis-cmd-i zrem (_key_ _member_))

; If the member already exists increment its score by _increment_, otherwise add the member setting _increment_ as score
(redis-cmd-i-fixed zincrby (_key_ _increment_ _member_))

; Return the rank (or index) or _member_ in the sorted set at _key_, with scores being ordered from low to high
; NB: Docs say this returns bulk, but we treat it as returning an integer
(redis-cmd-i-fixed zrank (_key_ _member_))

; Return the rank (or index) or _member_ in the sorted set at _key_, with scores being ordered from high to low
; NB: Docs say this returns bulk, but we treat it as returning an integer
(redis-cmd-i-fixed zrevrank (_key_ _member_))

; Return a range of elements from the sorted set at key
(redis-cmd-m-fixed zrange (_key_ _start_ _end_))

; Return a range of elements from the sorted set at key, exactly like ZRANGE, but the sorted set is ordered in traversed in reverse order, from the greatest to the smallest score
(redis-cmd-m-fixed zrevrange (_key_ _start_ _end_))

; Return all the elements with score >= min and score <= max (a range query) from the sorted set
(redis-cmd-m-fixed zrangebyscore (_key_ _min_ _max_))

(redis-cmd-m-fixed zrevrangebyscore (_key_ _max_ _min_))

; Count the number of elements of a sorted set with a score that lays within a given interval
(redis-cmd-i-fixed zcount (_key_ _lower_score_ _upper_score_))

; Return the cardinality (number of elements) of the sorted set at key
(redis-cmd-i-fixed zcard (_key_))

; Return the score associated with the specified element of the sorted set at key
(redis-cmd-b-fixed zscore (_key_ _element_))

; Remove all the elements with rank >= min and rank <= max from the sorted set
(redis-cmd-i-fixed zremrangebyrank (_key_ _min_ _max_))

; Remove all the elements with score >= min and score <= max from the sorted set
(redis-cmd-i-fixed zremrangebyscore (_key_ _min_ _max_))

;; ER-ONLY functions.
(redis-cmd-m-kl-fixed zrange (_key_ _start_ _end_ withscores))
(redis-cmd-m-kl-fixed zrevrange (_key_ _start_ _end_ withscores))
(redis-cmd-m-kl zrangebyscore (_key_ _min_ _max_ withscores)) ; also [LIMIT offset count]
(redis-cmd-m-kl zrevrangebyscore (_key_ _max_ _min_ withscores)) ; also [LIMIT offset count]
;; END ER_ONLY functions.

; Perform a union or intersection over a number of sorted sets with optional weight and aggregate`
; _dstkey_ _N_ _key1_ ... _keyN_ WEIGHTS _w1_ ... _wN_ AGGREGATE SUM|MIN|MAX
(redis-cmd-i zunionstore (_dstkey_ _n_ _key-spec_))
(redis-cmd-i zinterstore (_dstkey_ _n_ _key-spec_))

;; Commands operating on hashes ;;

; Set the hash field to the specified value. Creates the hash if needed.
(redis-cmd-i-tf-fixed hset (_key_ _field_ _value_))

; Retrieve the value of the specified hash field.
(redis-cmd-b-fixed hget (_key_ _field_))

; Set the hash fields to their respective values.
; _key1_ _field1_ ... _fieldN_
(redis-cmd-m hmget (_key_ _fields_))

; Set the hash fields to their respective values.
; _key_ _field1_ _value1_ ... _fieldN_ _valueN_
(redis-cmd-s hmset (_key_ _field-value-pairs_))

; Increment the integer value of the hash at _key_ on _field_ with _integer_.
(redis-cmd-i hincrby (_key_ _field_ _integer_))

; Test for existence of a specified field in a hash
(redis-cmd-i-tf-fixed hexists (_key_ _field_))

; Remove the specified field from a hash
(redis-cmd-i-tf hdel (_key_ _field_))

; Return the number of items in a hash.
(redis-cmd-i-fixed hlen (_key_))

; Return all the fields in a hash.
(redis-cmd-m-fixed hkeys (_key_))

; Return all the values in a hash.
(redis-cmd-m-fixed hvals (_key_))

; Return all the fields and associated values in a hash.
(redis-cmd-m-fixed hgetall (_key_))

;; ER-ONLY functions.
; Return all the fields of a hash as a proplist (e.g. [{atom, Binary}])
(redis-cmd-m-pl-fixed hgetall_p hgetall (_key_))
; Return all the fields of a hash as a keylist (e.g. [{Binary, Binary}])
(redis-cmd-m-kl-fixed hgetall_k hgetall (_key_))
;; END ER_ONLY functions.

;; Sorting ;;

; Sort a Set or a List accordingly to the specified parameters`
; _key_ BY _pattern_ LIMIT _start_ _end_ GET _pattern_ ASC|DESC ALPHA
(redis-cmd-m sort (_key_ _sort-definition_))

;; Transactions ;;

; Redis atomic transactions
(redis-cmd-s-fixed multi)
(redis-cmd-o-fixed exec) ; return the redis return values without translation (?)
(redis-cmd-s-fixed discard)

;; Publish/Subscribe  ;;

; Redis Public/Subscribe messaging paradigm implementation
(redis-cmd-strip subscribe (_channels_))
(redis-cmd-o unsubscribe (_channels_))
(redis-cmd-o-fixed unsubscribe)
(redis-cmd-strip psubscribe (_channel-patterns_))
(redis-cmd-o punsubscribe (_channel-patterns_))
(redis-cmd-o-fixed punsubscribe)
(redis-cmd-i-fixed publish (_channel_ _msg_))

;; Persistence control commands ;;

; Synchronously save the DB on disk
(redis-cmd-s-fixed save)

; Asynchronously save the DB on disk
(redis-cmd-s-fixed bgsave)

; Get and/or set configuration parameters
(redis-cmd-s-fixed config (_operation_)) ; covers RESETSTAT | REWRITE
(redis-cmd-b-fixed config (_get_ _param_))
(redis-cmd-s-fixed config (_set _param_ _value_))

; Return the UNIX time stamp of the last successfully saving of the dataset on disk
(redis-cmd-i-fixed lastsave)

; Synchronously save the DB on disk, then shutdown the server
(redis-cmd-s-fixed shutdown)

; Rewrite the append only file in background when it gets too big
(redis-cmd-s-fixed bgrewriteaof)


;; Remote server control commands ;;

; Provide information and statistics about the server
(redis-cmd-b-fixed info)

; Dump all the received requests in real time
(redis-cmd-o-fixed monitor)

; Change the replication settings
(redis-cmd-s-fixed slaveof (_ip_ _port_))

;; bulk addition of new untested commands

;; Cluster / Replication commands
(newcmd asking 0)
(newcmd cluster -1)
(newcmd migrate -5)
(newcmd readonly 0)
(newcmd readwrite 0)
(newcmd replconf 0)
(newcmd restore -3)
(newcmd sync 0)
(newcmd time 0)
(newcmd restore-asking -3)
(newcmd watch -1)
(newcmd unwatch 0)
(newcmd wait 2)  ; you shouldn't call wait directly

;; Debugging / Info
(newcmd debug -1)
(redis-cmd-strip-fixed dump (_key_))
(redis-cmd-b-fixed client (_op_)) ; GETNAME | LIST
(redis-cmd-b-fixed client (_op_ _arg_)) ; SETNAME name | KILL ip:port
(redis-cmd-m object (_type-descriptor_))
(newcmd slowlog -1)

;; scanning
(newcmd hscan -2)
(newcmd sscan -2)
(newcmd zscan -2)
(newcmd scan -1)

;; bit ops
(newcmd bitcount -1)
(newcmd bitop -3)

;; scripting
(newcmd eval -2)
(newcmd evalname -2)
(newcmd evalsha -2)
(newcmd script -1)
(newcmd spsubscribe -2)
(newcmd spunsubscribe -2)
(newcmd ssubscribe -2)
(newcmd sunsubscribe -2)

;; hash additions
(redis-cmd-i-fixed hsetnx (_key_ _field_ _nxval_))
(redis-cmd-f-fixed hincrbyfloat (_key_ _field_ _incrby_))

;; other floaty ops
(redis-cmd-f-fixed incrbyfloat (_key_ _incrby_))

;; pops
(newcmd pexpire 2)
(newcmd pexpireat 2)
(newcmd psetex 3)
(newcmd psync 2)
(newcmd pttl 1)

;; pubsub
(newcmd pubsub -1)

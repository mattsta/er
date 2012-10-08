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
(redis-cmd-s auth)

;; Connection commands ;;

; ping the server
(redis-cmd-s ping)

; echo the given string
(redis-cmd-b echo (_msg_))

;; Commands operating on all the kind of values ;;

; test if a key exists
; returns true on exists; false on not exists
(redis-cmd-i-tf exists (_key_))

; delete a key
; returns number of keys deleted: 0 to N
(redis-cmd-i del (_keys_))

; return the type of the value stored at key
(redis-cmd-s type (_key_))

; return all the keys matching a given pattern
(redis-cmd-b keys (_pattern_))

; return a random key from the key space
(redis-cmd-l randomkey)

; rename the old key in the new one, destroing the newname key if it already exists
; > er:rename(C, bob2, bob3).
; ** exception throw: {redis_error,<<"ERR no such key">>}
; > er:rename(C, bob3, bob3).
; ** exception throw: {redis_error,<<"ERR source and destination objects are the same">>}
(redis-cmd-s rename (_oldname_ _newname_))

; rename the old key in the new one, if the newname key does not already exist
(redis-cmd-i-tf renamenx (_oldname_ _newname_))

; return the number of keys in the current db
(redis-cmd-i dbsize)

; set a time to live in seconds on a key
(redis-cmd-i-tf expire (_key_ _seconds-forward_))
(redis-cmd-i-tf expireat (_key_ _unixtime_))

; remove a previously set expire
(redis-cmd-i-tf persist (_key_))

; get the time to live in seconds of a key
(redis-cmd-i ttl (_key_))

; Select the DB having the specified index
(redis-cmd-s select (_index_))

; Move the key from the currently selected DB to the DB having as index dbindex
(redis-cmd-i move (_key_ _dbindex_))

; Remove all the keys of the currently selected DB
(redis-cmd-s flushdb)

; Remove all the keys from all the databases
(redis-cmd-s flushall)


;; Commands operating on string values ;;

; set a key to a string value
(redis-cmd-s set (_key_ _value_))

; return the string value of the key
(redis-cmd-b get (_key_))

; set a key to a string returning the old value of the key
(redis-cmd-b getset (_key_ _value_))

; multi-get, return the strings values of the keys
; _key1_ _key2_ ... _keyN_
(redis-cmd-m mget (_keys_))

; set a key to a string value if the key does not exist
(redis-cmd-i-tf setnx (_key_ _value_))

; Set+Expire combo command
(redis-cmd-s setex (_key_ _time_ _value_))

; set a multiple keys to multiple values in a single atomic operation
; _key1_ _value1_ _key2_ _value2_ ... _keyN_ _valueN_
(redis-cmd-s mset (_key-value-pairs_))

; set a multiple keys to multiple values in a single atomic operation if none of the keys already exist
; _key1_ _value1_ _key2_ _value2_ ... _keyN_ _valueN_
(redis-cmd-i-tf msetnx (_key-value-pairs_))

; increment the integer value of key
(redis-cmd-i incr (_key_))

; increment the integer value of key by integer
(redis-cmd-i incrby (_key_ _integer_))

; decrement the integer value of key
(redis-cmd-i decr (_key_))

; decrement the integer value of key by integer
(redis-cmd-i decrby (_key_ _integer_))

; append the specified string to the string stored at key
(redis-cmd-i append (_key_ _value_))

; return a substring out of a larger string
(redis-cmd-b substr (_key_ _start_ _end_))   ; substr = getrange in 2.2+
(redis-cmd-b getrange (_key_ _start_ _end_)) ; getrange = substr for redis 2.2+

(redis-cmd-i setrange (_key_ _start_ _end_))

(redis-cmd-i getbit (_key_ _position_))
(redis-cmd-i setbit (_key_ _position_ _value_))

; return the length of a string
(redis-cmd-i strlen (_key_))

;; Commands operating on lists ;;

; Append an element to the tail of the List value at key
(redis-cmd-i rpush (_key_ _value_))

; Append an element to the head of the List value at key
(redis-cmd-i lpush (_key_ _value_))

; Return the length of the List value at key
(redis-cmd-i llen (_key_))

; Return a range of elements from the List at key
(redis-cmd-m lrange (_key_ _start_ _end_))

; Trim the list at key to the specified range of elements
(redis-cmd-s ltrim (_key_ _start_ _end_))

; Return the element at index position from the List at key
(redis-cmd-b lindex (_key_ _index_))

; Push on the left if the list exists.
; Returns number of elements in list.  Returns zero if list isn't created.
(redis-cmd-i lpushx (_key_ _value_))

; Push on the right if the list exists.
; Returns number of elements in list.  Returns zero if list isn't created.
(redis-cmd-i rpushx (_key_ _value_))

; Insert before or after a value in a list
; Returns the number of elements in list
(redis-cmd-i linsert (_key_ _before_or_after_ _existing_value_ _new_value_))

; Set a new value as the element at index position of the List at key
(redis-cmd-s lset (_key_ _index_ _value_))

; Remove the first-N, last-N, or all the elements matching value from the List at key
(redis-cmd-i lrem (_key_ _count_ _value_))

; Return and remove (atomically) the first element of the List at key
(redis-cmd-b lpop (_key_))

; Return and remove (atomically) the last element of the List at key
(redis-cmd-b rpop (_key_))

; Blocking LPOP
; _key1_ _key2_ ... _keyN_ _timeout_
(redis-cmd-m blpop (_keys_ _timeout_))

; Blocking RPOP
; _key1_ _key2_ ... _keyN_ _timeout_
(redis-cmd-m brpop (_keys_ _timeout_))

; Return and remove (atomically) the last element of the source List stored at _srckey_ and push the same element to the destination List stored at _dstkey_
(redis-cmd-b rpoplpush (_srckey_ _dstkey_))

; Blocking rpoplpush
(redis-cmd-b brpoplpush (_srckey_ _dstkey_))



;; Commands operating on sets ;;

; Add the specified member to the Set value at key
(redis-cmd-i-tf sadd (_key_ _member_))

; Remove the specified member from the Set value at key
(redis-cmd-i-tf srem (_key_ _member_))

; Remove and return (pop) a random element from the Set value at key
(redis-cmd-b spop (_key_))

; Move the specified member from one Set to another atomically
(redis-cmd-i-tf smove (_srckey_ _dstkey_ _member_))

; Return the number of elements (the cardinality) of the Set at key
(redis-cmd-i scard (_key_))

; Test if the specified value is a member of the Set at key
(redis-cmd-i-tf sismember (_key_ _member_))

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
(redis-cmd-m smembers (_key_))

; Return a random member of the Set value at key
(redis-cmd-b srandmember (_key_))


;; Commands operating on sorted sets (zsets, Redis version >= 1.1) ;;

; Add the specified member to the Sorted Set value at key or update the score if it already exist
(redis-cmd-i zadd (_key_ _score_ _member_))

; Remove the specified member from the Sorted Set value at key
(redis-cmd-i zrem (_key_ _member_))

; If the member already exists increment its score by _increment_, otherwise add the member setting _increment_ as score
(redis-cmd-i zincrby (_key_ _increment_ _member_))

; Return the rank (or index) or _member_ in the sorted set at _key_, with scores being ordered from low to high
; NB: Docs say this returns bulk, but we treat it as returning an integer
(redis-cmd-i zrank (_key_ _member_))

; Return the rank (or index) or _member_ in the sorted set at _key_, with scores being ordered from high to low
; NB: Docs say this returns bulk, but we treat it as returning an integer
(redis-cmd-i zrevrank (_key_ _member_))

; Return a range of elements from the sorted set at key
(redis-cmd-m zrange (_key_ _start_ _end_))

; Return a range of elements from the sorted set at key, exactly like ZRANGE, but the sorted set is ordered in traversed in reverse order, from the greatest to the smallest score
(redis-cmd-m zrevrange (_key_ _start_ _end_))

; Return all the elements with score >= min and score <= max (a range query) from the sorted set
(redis-cmd-m zrangebyscore (_key_ _min_ _max_))

; Count the number of elements of a sorted set with a score that lays within a given interval
(redis-cmd-i zcount (_key_ _lower_score_ _upper_score_))

; Return the cardinality (number of elements) of the sorted set at key
(redis-cmd-i zcard (_key_))

; Return the score associated with the specified element of the sorted set at key
(redis-cmd-b zscore (_key_ _element_))

; Remove all the elements with rank >= min and rank <= max from the sorted set
(redis-cmd-i zremrangebyrank (_key_ _min_ _max_))

; Remove all the elements with score >= min and score <= max from the sorted set
(redis-cmd-i zremrangebyscore (_key_ _min_ _max_))

;; ER-ONLY functions.
(redis-cmd-m-kl zrange zrange (_key_ _start_ _end_ withscores))
(redis-cmd-m-kl zrevrange zrevrange (_key_ _start_ _end_ withscores))
(redis-cmd-m-kl zrangebyscore zrangebyscore (_key_ _min_ _max_ withscores))
;; END ER_ONLY functions.

; Perform a union or intersection over a number of sorted sets with optional weight and aggregate`
; _dstkey_ _N_ _key1_ ... _keyN_ WEIGHTS _w1_ ... _wN_ AGGREGATE SUM|MIN|MAX
(redis-cmd-i zunionstore (_dstkey_ _n_ _key-spec_))
(redis-cmd-i zinterstore (_dstkey_ _n_ _key-spec_))

;; Commands operating on hashes ;;

; Set the hash field to the specified value. Creates the hash if needed.
(redis-cmd-i-tf hset (_key_ _field_ _value_))

; Retrieve the value of the specified hash field.
(redis-cmd-b hget (_key_ _field_))

; Set the hash fields to their respective values.
; _key1_ _field1_ ... _fieldN_
(redis-cmd-m hmget (_key_ _fields_))

; Set the hash fields to their respective values.
; _key_ _field1_ _value1_ ... _fieldN_ _valueN_
(redis-cmd-s hmset (_key_ _field-value-pairs_))

; Increment the integer value of the hash at _key_ on _field_ with _integer_.
(redis-cmd-i hincrby (_key_ _field_ _integer_))

; Test for existence of a specified field in a hash
(redis-cmd-i-tf hexists (_key_ _field_))

; Remove the specified field from a hash
(redis-cmd-i-tf hdel (_key_ _field_))

; Return the number of items in a hash.
(redis-cmd-i hlen (_key_))

; Return all the fields in a hash.
(redis-cmd-m hkeys (_key_))

; Return all the values in a hash.
(redis-cmd-m hvals (_key_))

; Return all the fields and associated values in a hash.
(redis-cmd-m hgetall (_key_))

;; ER-ONLY functions.
; Return all the fields of a hash as a proplist (e.g. [{atom, Binary}])
(redis-cmd-m-pl hgetall_p hgetall (_key_))
; Return all the fields of a hash as a keylist (e.g. [{Binary, Binary}])
(redis-cmd-m-kl hgetall_k hgetall (_key_))
;; END ER_ONLY functions.

;; Sorting ;;

; Sort a Set or a List accordingly to the specified parameters`
; _key_ BY _pattern_ LIMIT _start_ _end_ GET _pattern_ ASC|DESC ALPHA
(redis-cmd-m sort (_key_ _sort-definition_))

;; Transactions ;;

; Redis atomic transactions
(redis-cmd-s multi)
(redis-cmd-o exec) ; return the redis return values without translation (?)
(redis-cmd-s discard)

;; Publish/Subscribe  ;;

; Redis Public/Subscribe messaging paradigm implementation
(redis-cmd-strip subscribe (_channels_))
(redis-cmd-o unsubscribe (_channels_))
(redis-cmd-o unsubscribe)
(redis-cmd-strip psubscribe (_channel-patterns_))
(redis-cmd-o punsubscribe (_channel-patterns_))
(redis-cmd-o punsubscribe)
(redis-cmd-i publish (_channel_ _msg_))

;; Persistence control commands ;;

; Synchronously save the DB on disk
(redis-cmd-s save)

; Asynchronously save the DB on disk
(redis-cmd-s bgsave)

; Get and/or set configuration parameters
(redis-cmd-s config (_getset_ _params_and_or_values_))

; Return the UNIX time stamp of the last successfully saving of the dataset on disk
(redis-cmd-i lastsave)

; Synchronously save the DB on disk, then shutdown the server
(redis-cmd-s shutdown)

; Rewrite the append only file in background when it gets too big
(redis-cmd-s bgrewriteaof)


;; Remote server control commands ;;

; Provide information and statistics about the server
(redis-cmd-b info)

; Dump all the received requests in real time
(redis-cmd-o monitor)

; Change the replication settings
(redis-cmd-s slaveof)

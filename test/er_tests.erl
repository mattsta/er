-module(er_tests).
-include_lib("eunit/include/eunit.hrl").

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).

redis_setup_clean() ->
  TestModule = er_redis,
  {ok, Cxn} = TestModule:start_link("127.0.0.1", 6991),
  {ok, _PoolPid} = er_pool:start_link(testing_pool, "127.0.0.1", 6991),
  ok = er:flushall(Cxn),
  Cxn.

redis_cleanup(Cxn) ->
  Cxn ! shutdown,
  testing_pool ! shutdown.

er_basic_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(C) -> 
      [
        ?_E(false, er:exists(C, existing)),
        ?_E(ok,    er:set(C, existing, ralph)),
        ?_E(true,  er:exists(C, existing)),
        ?_E(<<"ralph">>, er:get(C, existing)),
        ?_E(1,     er:del(C, existing)),
        ?_E(0,     er:del(C, existing)),
        ?_E(false, er:exists(C, existing)),
        ?_E([],    er:keys(C, "*")),
        ?_E(nil,   er:randomkey(C)),
        ?_E(ok,    er:set(C, <<>>, ralph)),
        ?_E(<<>>,  er:randomkey(C)),
        ?_E(<<"ralph">>, er:get(C, <<>>)),
        ?_E(1,     er:del(C, <<>>)),
        ?_assertException(throw,
          {redis_error, <<"ERR no such key">>},
          er:rename(C, bob2, bob3)),
        ?_E(ok, er:set(C, bob3, bob3content)),
        ?_assertException(throw,
          {redis_error,
            <<"ERR source and destination objects are the same">>},
          er:rename(C, bob3, bob3)),
        ?_E(ok,    er:rename(C, bob3, bob2)),
        ?_E(true,  er:renamenx(C, bob2, bob3)),
        ?_E(ok,    er:set(C, bob4, bob4content)),
        ?_E(false, er:renamenx(C, bob3, bob4)),
        ?_E(2,     er:dbsize(C)),
        ?_E(ok,    er:set(C, expireme, expiremecontent)),
        ?_E(true,  er:expire(C, expireme, 30)),
        ?_E(true,  er:expire(C, expireme, 30)),  % behavior changed in redis 2.2
        ?_E(false, er:expire(C, expireme_noexist, 30)),
        ?_assertMatch(TTL when TTL =:= 29 orelse TTL =:= 30,
          er:ttl(C, expireme)),
        ?_E(true,  er:setnx(C, abc123, abc)),
        ?_E(false, er:setnx(C, abc123, abc)),
        ?_E(false, er:msetnx(C, [abc123, abc, abc234, def])),
        ?_E(true,  er:msetnx(C, [abc234, def, abc567, hij])),
        % getset,
        % mget,
        % setex,
        % mset,
        % incr,
        % incrby,
        % decr,
        % decrby,
        % append,

        % bitkey: 01000000 = @
        ?_E(0, er:setbit(C, bitkey, 1, 1)),
        ?_E(<<01000000:8>>, er:get(C, bitkey)),
        ?_E(1, er:getbit(C, bitkey, 1)),
        ?_E(0, er:getbit(C, bitkey, 0)),
        ?_E(0, er:getbit(C, bitkey, 12)),
        ?_E(0, er:getbit(C, bitkey, 32)),
        ?_E(0, er:getbit(C, bitkey, 64)),
        ?_E(0, er:getbit(C, bitkey, 999)),
        % test setting arbitrarily large index
        ?_E(0, er:setbit(C, bitkey, 1024968, 1)),
        ?_E(0, er:getbit(C, bitkey, 1024967)),
        ?_E(1, er:getbit(C, bitkey, 1024968)),
        ?_E(0, er:getbit(C, bitkey, 1024969)),
        % binarykey
        ?_E(0, er:setbit(C, binarykey, 0, 1)),
        % 10000000 = 128
        ?_E(<<128>>, er:get(C, binarykey)),
        ?_E(0,    er:setbit(C, binarykey, 7, 1)),
        % 10000001 = 128
        ?_E(<<129>>, er:get(C, binarykey)),
        ?_E(0,    er:setbit(C, binarykey, 64, 1)),
        % binarykey = (see next three lines)
        % [10000001]
        % [00000000][00000000][00000000][00000000][00000000][00000000][00000000]
        % [1000000]
        % = 129, 8x0, 8x0, 8x0, 8x0, 8x0, 8x0, 8x0, 128
        ?_E(<<129,0,0,0,0,0,0,0,128>>, er:get(C, binarykey)),
        % binaryint
        % 5 = 00000101
        % *NB:* Binary 5 is not the same as ASCII 5.
        ?_E(ok,er:set(C, binaryint, <<5>>)),
        ?_E(0, er:getbit(C, binaryint, 0)),
        ?_E(0, er:getbit(C, binaryint, 1)),
        ?_E(0, er:getbit(C, binaryint, 2)),
        ?_E(0, er:getbit(C, binaryint, 3)),
        ?_E(0, er:getbit(C, binaryint, 4)),
        ?_E(1, er:getbit(C, binaryint, 5)),
        ?_E(0, er:getbit(C, binaryint, 6)),
        ?_E(1, er:getbit(C, binaryint, 7)),
        % nothinghere.  substr got renamed getrange in redis 2.2
        % getrange is now just an alias to substr in redis
        ?_E(nil, er:getrange(C, nothinghere, 1, 3)),
        ?_E(nil, er:substr(C, nothinghere, 1, 3)),
        % rangekey tests
        ?_E(ok,  er:set(C, rangekey, "Hello")),
        ?_E(<<"ell">>, er:getrange(C, rangekey, 1, 3)),
        ?_E(<<"ell">>, er:substr(C, rangekey, 1, 3)),
        ?_E(5, er:setrange(C, rangekey, 1, "no")),
        ?_E(<<"nolo">>, er:getrange(C, rangekey, 1, 4)),
        % zero padding happens when adding to a nonexistent key
        ?_E(13, er:setrange(C, rangeemptykey, 3, "Empty Test")),
        ?_E(<<0,0,0,"Empty Test">>, er:get(C, rangeemptykey)),
        % reading past the length of a string
        ?_E(nil, er:getrange(C, rangekey, 64, 32)),
        ?_E(nil, er:substr(C, rangekey, 64, 32))
      ]
    end
  }.

er_lists_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(C) -> 
      [
        % []
        ?_E(1,  er:rpush(C, listA, aitem1)),
        % aitem1
        ?_E(2,  er:rpush(C, listA, aitem2)),
        % aitem1, aitem2
        ?_E(3,  er:rpush(C, listA, aitem3)),
        % aitem1, aitem2, aitem3
        ?_E(4,  er:lpush(C, listA, aitem4)),
        % aitem4, aitem1, aitem2, aitem3
        ?_E(5,  er:lpush(C, listA, aitem5)),
        % aitem5, aitem4, aitem1, aitem2, aitem3
        ?_E(6,  er:lpush(C, listA, aitem6)),
        % aitem6, aitem5, aitem4, aitem1, aitem2, aitem3
        ?_E(6, er:llen(C, listA)),
        ?_E([<<"aitem6">>], er:lrange(C, listA, 0, 0)),
        ?_E([<<"aitem6">>, <<"aitem5">>], er:lrange(C, listA, 0, 1)),
        ?_E([], er:lrange(C, listA, 10, 20)),
        ?_E(7, er:lpushx(C, listA, aitem7)),
        % aitem7, aitem6, aitem5, aitem4, aitem1, aitem2, aitem3
        ?_E(8, er:rpushx(C, listA, aitem3)),
        % aitem7, aitem6, aitem5, aitem4, aitem1, aitem2, aitem3
        ?_E(0, er:lpushx(C, noneList, aitemZ)),
        ?_E(0, er:rpushx(C, noneList, aitemZ)),
        ?_E(9, er:linsert(C, listA, before, aitem5, aitemNew)),
        % aitem7, aitem6, aitemNew, aitem5, aitem4, aitem1, aitem2, aitem3, aitemZ
        ?_E(10, er:linsert(C, listA, 'after', aitem1, aitemNew2)),
        % aitem7, aitem6, aitemNew, aitem5, aitem4, aitem1, aitemNew2, aitem2, aitem3, aitemZ
        ?_E(<<"aitem6">>, er:lindex(C, listA, 1)),
        ?_E(<<"aitem1">>, er:lindex(C, listA, 5))
        % ltrim
        % lset
        % lrem
        % lpop
        % rpop
        % blpop
        % brpop
        % rpoplpush
        % brpoplpush
      ]
    end
  }.

er_sets_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(C) -> 
      [
        ?_E(true,  er:sadd(C, setA, amember1)),
        ?_E(false, er:sadd(C, setA, amember1)),
        ?_E(true,  er:sadd(C, setA, amember2)),
        ?_E(true,  er:sadd(C, setA, amember3)),
        ?_E(true,  er:srem(C, setA, amember1)),
        ?_E(false, er:srem(C, setA, amember1)),
        ?_assertMatch(M when M =:= <<"amember2">> orelse M =:= <<"amember3">>,
          er:spop(C, setA)),
        ?_assertMatch(M when M =:= <<"amember2">> orelse M =:= <<"amember3">>,
          er:spop(C, setA)),
        ?_E(nil,   er:spop(C, setA)),
        ?_E(0,     er:scard(C, setA)),
        ?_E(true,  er:sadd(C, setB, bmember1)),
        ?_E(true,  er:sadd(C, setB, bmember2)),
        ?_E(true,  er:sadd(C, setB, bmember3)),
        ?_E(3,     er:scard(C, setB)),
        ?_E(false, er:smove(C, setB, setA, bmembernone)),
        ?_E(true,  er:smove(C, setB, setA, bmember1)),
        ?_E(1,     er:scard(C, setA)),
        ?_E(2,     er:scard(C, setB)),
        ?_E(true,  er:sismember(C, setB, bmember2)),
        ?_E(false, er:sismember(C, setB, bmember9))
        % sinter
        % sinterstore
        % sunion
        % sunionstore
        % sdiff
        % sdiffstore
        % smembers
        % srandmember
      ]
    end
  }.

er_sorted_sets_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(C) -> 
      [
        ?_E(true,  er:zadd(C, zsetA, 10, amember1)),
        ?_E(false, er:zadd(C, zsetA, 10, amember1)),
        ?_E(true,  er:zadd(C, zsetA, 10, amember2)),
        ?_E(true,  er:zadd(C, zsetA, 10, amember3)),
        ?_E(true,  er:zrem(C, zsetA, amember3)),
        ?_E(false, er:zrem(C, zsetA, amembernone)),
        ?_E(20,    er:zincrby(C, zsetA, 10, amember1)),
        ?_E(-20,   er:zincrby(C, zsetA, -40, amember1)),
        ?_E(0,     er:zrank(C, zsetA, amember1)),
        ?_E(1,     er:zrank(C, zsetA, amember2)),
        ?_E(1,     er:zrevrank(C, zsetA, amember1)),
        ?_E(0,     er:zrevrank(C, zsetA, amember2)),
        ?_E(inf,   er:zincrby(C, zsetA, "inf", amember1)),
        ?_E(inf,   er:zincrby(C, zsetA, "inf", amember1)),
        ?_E(1,     er:zrank(C, zsetA, amember1)),  % -20 + inf = top of list
        ?_E(0,     er:zrank(C, zsetA, amember2)),
        ?_assertException(throw,
          {redis_error, <<"ERR resulting score is not a number (NaN)">>},
          er:zincrby(C, zsetA, "-inf", amember1)),
        ?_assertException(throw,
          {redis_error, <<"ERR resulting score is not a number (NaN)">>},
          er:zincrby(C, zsetA, "-inf", amember1)),
        ?_E(1,    er:zrank(C, zsetA, amember1)),  % at position inf, it moves up
        ?_E(0,    er:zrank(C, zsetA, amember2)),
        ?_E([<<"amember2">>, <<"amember1">>], er:zrange(C, zsetA, 0, 3)),
        ?_E([{<<"amember2">>, <<"10">>},
             {<<"amember1">>, <<"inf">>}], er:zrange(C, zsetA, 0,3,withscores)),
        ?_E([<<"amember1">>, <<"amember2">>], er:zrevrange(C, zsetA, 0, 3)),
        ?_E([{<<"amember1">>, <<"inf">>},
             {<<"amember2">>, <<"10">>}], er:zrevrange(C,zsetA,0,3,withscores))
        % zrangebyscore
        % zcard
        % zscore
        % zremrangebyrank
        % zremrangebyscore
        % zunionstore
        % zinterstore
      ]
    end
  }.

er_hashes_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(C) -> 
      [
        ?_E(true,  er:hset(C, hashA, fieldA, valueA)),
        ?_E(true,  er:hset(C, hashA, fieldB, valueB)),
        ?_E(true,  er:hset(C, hashA, fieldC, valueC)),
        ?_E(false, er:hset(C, hashA, fieldA, valueA1)),
        ?_E(<<"valueA1">>, er:hget(C, hashA, fieldA)),
        ?_E(nil,   er:hget(C, hashB, fieldZ)),
        ?_E([nil, <<"valueA1">>, <<"valueC">>],
                     er:hmget(C, hashA, [fieldNone, fieldA, fieldC])),
        ?_E([<<"valueA1">>, <<"valueC">>, nil],
                     er:hmget(C, hashA, [fieldA, fieldC, fieldNone])),
        ?_E([nil, <<"valueA1">>, nil, <<"valueC">>, nil],
                     er:hmget(C, hashA, [fieldZombie, fieldA, 
                                         fieldDead, fieldC,
                                         fieldNone])),
        ?_E([<<"valueA1">>, <<"valueC">>],
                     er:hmget(C, hashA, [fieldA, fieldC])),
        ?_E([nil], er:hmget(C, hashNone, [fieldNothing])),
        ?_E(ok,    er:hmset(C, hashC, [fieldA, [], fieldB, valB])),
        ?_E(ok,    er:hmset(C, hashC, [fieldA, valA, fieldB, valB])),
        ?_E(<<"valA">>, er:hget(C, hashC, fieldA)),
        ?_E(<<"valB">>, er:hget(C, hashC, fieldB)),
        ?_E(12,    er:hincrby(C, hashD, fieldAddr, 12)),
        ?_E(72,    er:hincrby(C, hashD, fieldAddr, 60)),
        ?_E(true,  er:hexists(C, hashD, fieldAddr)), 
        ?_E(false, er:hexists(C, hashD, fieldBddr)), 
        ?_E(false, er:hexists(C, hashZ, fieldZ)),
        ?_E(true,  er:hdel(C, hashD, fieldAddr)),
        ?_E(false, er:hdel(C, hashD, fieldAddr)),
        ?_E(3,     er:hlen(C, hashA)),
        ?_E([<<"fieldA">>, <<"fieldB">>, <<"fieldC">>],
                   er:hkeys(C, hashA)),
        ?_E([<<"valueA1">>, <<"valueB">>, <<"valueC">>],
                   er:hvals(C, hashA)),
        ?_E([<<"fieldA">>, <<"valueA1">>,
             <<"fieldB">>, <<"valueB">>,
             <<"fieldC">>, <<"valueC">>],
                   er:hgetall(C, hashA)),
        ?_E([{fieldA, <<"valueA1">>},
             {fieldB, <<"valueB">>},
             {fieldC, <<"valueC">>}],
                   er:hgetall_p(C, hashA)),
        ?_E([{<<"fieldA">>, <<"valueA1">>},
             {<<"fieldB">>, <<"valueB">>},
             {<<"fieldC">>, <<"valueC">>}],
                   er:hgetall_k(C, hashA)),
        ?_E(ok,    er:hmset(C, hashHasEmpty, [fieldA, valA, fieldB, <<"">>])),
        ?_E([<<"fieldA">>, <<"valA">>,
             <<"fieldB">>, <<"">>],
                   er:hgetall(C, hashHasEmpty))
      ]
    end
  }.

er_sorting_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(_C) -> 
      [
        % sort
      ]
    end
  }.

er_transactions_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(IndividualRedis) ->
      TxnA = fun(C) ->
               queued = er:setnx(C, bob, four),
               queued = er:setnx(C, bob, three)
             end,
      TxnB = fun(C) ->
               er:incr(C, bob),
               er:setnx(C, bob, three)
             end,
      TxnC = fun(C) ->
               queued = er:incr(C, incrementer),
               er:discard(C)
             end,
      [
        % 1 = success, 0 = failure for setnx
        ?_E([1, 0], er:er_transaction(testing_pool, TxnA)),
        ?_E([0, 0], er:er_transaction(IndividualRedis, TxnA)),
        % Errors during a MULTI are thrown.  Since the entire multi wasn't run,
        % we don't care about the individual statuses from other returns.
        ?_assertException(throw,
          {redis_error,<<"ERR value is not an integer or out of range">>},
          er:er_transaction(testing_pool, TxnB)),
        ?_assertException(throw,
          {redis_error,<<"ERR value is not an integer or out of range">>},
          er:er_transaction(IndividualRedis, TxnB)),
        ?_E(discarded, er:er_transaction(testing_pool, TxnC)),
        ?_E(discarded, er:er_transaction(IndividualRedis, TxnC))
        % watch -- test concurrently
        % unwatch -- test concurrently
      ]
    end
  }.

er_pubsub_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(_C) -> 
      [
        % subscribe
        % unsubscribe
        % psubscribe
        % punsubscribe
        % publish
      ]
    end
  }.

er_persistence_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(_C) -> 
      [
        % save
        % bgsave
        % lastsave
        % shutdown
        % bgrewriteaof
      ]
    end
  }.

er_server_control_commands_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(_C) -> 
      [
        % info
        % monitor
        % slaveof
        % config
      ]
    end
  }.

er_return_test_() ->
  {inparallel, 
    [
      ?_E(nil,      er:'redis-return-nil'(nil)),
      ?_E(ok,       er:'redis-return-status'([<<"ok">>])),
      ?_assertException(throw, {redis_error, <<"throwed">>},
        er:'redis-return-status'({error, <<"throwed">>})),
      ?_E(53,       er:'redis-return-integer'([<<"53">>])),
      ?_E(inf,      er:'redis-return-integer'([<<"inf">>])),
      ?_E('-inf',   er:'redis-return-integer'([<<"-inf">>])),
      ?_E(<<"ok">>, er:'redis-return-single-line'([<<"ok">>])),
      ?_E(ok,       er:'redis-return-bulk'(ok)),
      ?_E(ok,       er:'redis-return-multibulk'(ok)),
      ?_E(nil,      er:'redis-return-multibulk'({ok, nil})),
      ?_E(ok,       er:'redis-return-special'(ok)),
      ?_E(true,     er:'redis-return-integer-true-false'([<<"1">>])),
      ?_E(false,    er:'redis-return-integer-true-false'([<<"0">>])),
      ?_E([],       er:'redis-return-multibulk-pl'([])),
      ?_E([],       er:'redis-return-multibulk-kl'([])),
      ?_E([{hello, <<"ok">>}],
        er:'redis-return-multibulk-pl'([{ok, <<"hello">>}, {ok, <<"ok">>}])),
      ?_E([{hello, <<"ok">>}, {hello2, <<"ok2">>}],
        er:'redis-return-multibulk-pl'([{ok, <<"hello">>}, {ok, <<"ok">>},
                                        {ok, <<"hello2">>}, {ok, <<"ok2">>}])),
      ?_E([{<<"hello">>, <<"ok">>}],
        er:'redis-return-multibulk-kl'([{ok, <<"hello">>}, {ok, <<"ok">>}])),
      ?_E([{<<"hello">>, <<"ok">>}, {<<"hello2">>, <<"ok2">>}],
        er:'redis-return-multibulk-kl'([{ok, <<"hello">>}, {ok, <<"ok">>},
                                        {ok, <<"hello2">>}, {ok, <<"ok2">>}]))
    ]
  }.


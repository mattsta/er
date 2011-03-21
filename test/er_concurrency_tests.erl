-module(er_concurrency_tests).
-include_lib("eunit/include/eunit.hrl").
-import(lists, [flatten/1]).

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).
-define(_M(A, B), ?_assertMatch(A, B)).
% Easily enable/disable debug printing by swapping which ?d is used:
%-define(d(A, B), ?debugTime(A, B)).
-define(d(A, B), B).

kv_pairs() ->
  AtoZ = lists:seq($A, $Z),
  % Generate keyA - keyZ
  Keys = [<<"key", KeyId>> || KeyId <- AtoZ],
  % Generate valA - valZ
  Vals = [<<"val", KeyId>> || KeyId <- AtoZ],
  lists:zip(Keys, Vals).

redis_setup_clean() ->
  ConcurrencyMod = er_pool,  % er_pool,  inparallel = .4 to .6 seconds
                             % er_pool,  inorder    = 16 seconds
                             % er_redis, inparallel = 1.5 to 2 seconds
                             % er_redis, inorder    = 16 seconds
  Cxn = case ConcurrencyMod:start_link(er_pool, "127.0.0.1", 6991) of
          {ok, C} -> C;
          {error, {already_started, Pid}} -> Pid
        end,
  ok = er:flushall(Cxn),
  % Store (keyA, valA), (keyB, valB), ..., (keyZ, valZ)
  [er:set(Cxn, K, V) || {K, V} <- kv_pairs()],
  Cxn.

redis_cleanup(Cxn) ->
  Cxn ! shutdown.

er_concurrency_test_() ->
  {setup,
    fun redis_setup_clean/0,
    fun redis_cleanup/1,
    fun(C) ->
      [
        % Changing both tests to 'inorder' causes test() to take ~16 seconds.
        {inparallel,
           % generate and run 26 concurrent gets and make sure results work
          [?d(V, ?_E(V, er:get(C, K))) || {K, V} <- kv_pairs()]
        },
        {inparallel,
          % generate and run 26 * 200 = 5200 concurrent gets and make sure results work
          [[?d(V, ?_E(V, er:get(C, K))) || {K, V} <- kv_pairs()]
            || _ <- lists:seq(1, 200)]
        }
      ]
    end
  }.

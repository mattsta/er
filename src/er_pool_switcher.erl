-module(er_pool_switcher).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/2]).

-record(state, {pools :: [{pid(), list()}]  % Pid of er_pool and Args
               }).

%%====================================================================
%% api callbacks
%%====================================================================
start_link(GenServerName, PoolsWithArgs)
    when is_atom(GenServerName) andalso is_list(hd(PoolsWithArgs)) ->
  gen_server:start_link({local, GenServerName}, ?MODULE, PoolsWithArgs, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(PoolArgs) -> 
  process_flag(trap_exit, true),
  PoolPids = [pool(Args) || Args <- PoolArgs],
  attach_liveness_check(PoolPids),
  {ok, #state{pools = lists:zip(PoolPids, PoolArgs)}}.

% If current pid is a stale backup, try to bring live.
% If we can't bring it live, cycle to the next backup.
handle_call({cmd, Parts}, From, #state{pools = [{nil, Args}|T]} = State) ->
  case pool(Args) of
    nil -> NextPool = T ++ [{nil, Args}];  % switch to next backup
      P -> NextPool = [{P, Args} | T]      % this connect worked.  use it.
  end,
  handle_call({cmd, Parts}, From, State#state{pools = NextPool});

handle_call({cmd, Parts}, From, #state{pools = [{Pid, _}|_]} = State) ->
  spawn(fun() ->
          gen_server:reply(From, gen_server:call(Pid, {cmd, Parts}, infinity))
        end),
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

% An er_pool died.  If live, promote next-highest to live.
handle_info({'EXIT', DeadPid, _Reason},
    #state{pools = [{LivePid, Args} | T] = Pools} = State) ->
  case DeadPid of
     LivePid -> Connected = pool(Args),
                NewPools = T ++ [{Connected, Args}];
    OtherPid -> {OtherPid, OtherArgs} = lists:keyfind(OtherPid, 1, Pools),
                Connected = pool(OtherArgs),
                NewPoolEntry = {Connected, OtherArgs},
                NewPools = lists:keyreplace(OtherPid, 1, Pools, NewPoolEntry)
  end,
  attach_liveness_check(NewPools),
  {noreply, State#state{pools = NewPools}};

handle_info(Info, State) ->
  error_logger:error_msg("Other info: ~p with state ~p~n", [Info, State]),
  {noreply, State}.

terminate(_Reason, #state{pools = Pools}) ->
  [exit(Pid, normal) || {Pid, _} <- Pools].

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
pool(Args) ->
  try apply(er_pool, start_link_nameless, Args) of
    {ok, Pid} -> Pid
  catch
    throw:Error ->
      error_logger:error_msg("Connect failed: ~p ~p. Keeping stale.~n",
        [Error, Args]),
      nil
  end.

attach_liveness_check([{ErPid, _Args} | _T]) when is_list(ErPid) ->
  attach_liveness_check(ErPid, 250);
attach_liveness_check([ErPid | _T]) when is_pid(ErPid) ->
  attach_liveness_check(ErPid, 250).
 
% Ping against redis every IntervalMS milliseconds.
% If redis is down, the pool crashes.
attach_liveness_check(ErPool, IntervalMS) when is_pid(ErPool) ->
  timer:apply_interval(IntervalMS, er, ping, [ErPool]).

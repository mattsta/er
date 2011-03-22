-module(er_pool).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, start_link/1, start_link/3, start_link/4]).
-export([start_link_nameless/2, start_link_nameless/3, start_link_nameless/4]).

-record(state, {ip             :: string(),
                port           :: pos_integer(),
                available      :: [pid()],
                reserved       :: [pid()],
                error_strategy :: {retry, pos_integer()} | % retry count
                                  {wait, pos_integer()} |  % retry every-N ms
                                  crash
               }).

%%====================================================================
%% api callbacks
%%====================================================================
% With names
start_link() ->
  start_link(?MODULE).

start_link(GenServerName) when is_atom(GenServerName) ->
  start_link(GenServerName, "127.0.0.1", 6379).

start_link(GenServerName, IP, Port) when is_atom(GenServerName) ->
  start_link(GenServerName, IP, Port, 25).

start_link(GenServerName, IP, Port, SocketCount) when is_atom(GenServerName) ->
  start_link(GenServerName, IP, Port, SocketCount, crash).

start_link(GenServerName, IP, Port, SocketCount, Strategy)
    when is_atom(GenServerName) ->
  gen_server:start_link({local, GenServerName}, ?MODULE,
    [IP, Port, SocketCount, Strategy], []).

% Without names
start_link_nameless(IP, Port) ->
  start_link(IP, Port, 25).

start_link_nameless(IP, Port, SocketCount) ->
  start_link(IP, Port, SocketCount, crash).

start_link_nameless(IP, Port, SocketCount, Strategy) ->
  gen_server:start_link(?MODULE, [IP, Port, SocketCount, Strategy], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([IP, Port, SocketCount, Strategy]) when is_list(IP), is_integer(Port) ->
  process_flag(trap_exit, true),
  PreState = #state{ip = IP, port = Port, error_strategy = Strategy},
  try initial_connect(SocketCount, PreState) of
    State -> {ok, State}
  catch
    throw:Error -> {stop, Error}
  end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
% These commands persisit state on one connection.  We can't add
% their connection back to the general pool.
handle_call({cmd, Parts}, From,
    #state{available = [H|T], reserved = R} = State) when
    hd(Parts) =:= <<"multi">> orelse
    hd(Parts) =:= <<"watch">> orelse
    hd(Parts) =:= <<"subscribe">> orelse
    hd(Parts) =:= <<"psubscribe">> orelse
    hd(Parts) =:= <<"monitor">> ->
  spawn(fun() ->
          gen_server:reply(From, {H, gen_server:call(H, {cmd, Parts}, infinity)})
        end),
  Caller = self(),
  spawn(fun() -> Caller ! add_connection end),
  {noreply, State#state{available = T, reserved = [H | R]}};

% Blocking list ops *do* block, but don't need to return their er_redis pid
% Transactional returns should self-clean-up
handle_call({cmd, Parts}, From,
    #state{available = [H|T], reserved = R} = State) when
    hd(Parts) =:= <<"exec">> orelse
    hd(Parts) =:= <<"discard">> orelse
    hd(Parts) =:= <<"blpop">> orelse
    hd(Parts) =:= <<"brpoplpush">> orelse
    hd(Parts) =:= <<"brpop">> ->
  Caller = self(),
  spawn(fun() ->
          gen_server:reply(From, gen_server:call(H, {cmd, Parts}, infinity)),
          Caller ! {done_processing_reserved, H}
        end),
  spawn(fun() -> Caller ! add_connection end),
  {noreply, State#state{available = T, reserved = [H | R]}};

handle_call({cmd, Parts}, From,
    #state{available = [H|T]} = State) ->
  spawn(fun() ->
          gen_server:reply(From, gen_server:call(H, {cmd, Parts}, infinity))
        end),
  {noreply, State#state{available = T ++ [H]}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

% A blocking or reserved operation finished.  Add the reserved server
% back into the available pool.
handle_info({done_processing_reserved, Pid},
    #state{available=Available, reserved=Reserved} = State) ->
  RemovedOld = Reserved -- [Pid],
  NewAvail = [Pid | Available],
  {noreply, State#state{available=NewAvail, reserved=RemovedOld}};

% An er_redis died because of a connection error.  Do something.
% {wait, N} and {retry, N} are not perfect right now.
handle_info(add_connection, #state{available=Available} = State) ->
  try connect(State) of
    Connected -> {noreply, State#state{available=[Connected | Available]}}
  catch
    throw:Error -> run_error_strategy(Error, State)
  end;

% An er_redis died because of a connection error.  Do something.
% {wait, N} and {retry, N} are not perfect right now.
handle_info({'EXIT', _Pid, {er_connect_failed, _, _, _}} = Error, State) ->
  run_error_strategy(Error, State);

% An er_redis died because of some other error.  Remove it from list of servers.
handle_info({'EXIT', Pid, _Reason} = Err,
    #state{available=Available, reserved=Reserved} = State) ->
  try connect(State) of
    Connected ->
      case lists:member(Pid, Available) of
         true -> RemovedOld = Available -- [Pid],
                 NewAvail = [Connected | RemovedOld],
                 {noreply, State#state{available = NewAvail}};
        false -> RemovedOld = Reserved -- [Pid],
                 NewAvail = [Connected | Available],
                 {noreply, State#state{available = NewAvail,
                                       reserved = RemovedOld}}
      end
  catch
    throw:Error -> run_error_strategy(Error, State)
  end;

handle_info(shutdown, State) ->
  {stop, normal, State};

handle_info(Info, State) ->
  error_logger:error_msg("Other info: ~p with state ~p~n", [Info, State]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{available=Available, reserved=Reserved}) ->
  [exit(P, normal) || P <- Available],
  [exit(P, normal) || P <- Reserved].

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_connect(SockCount, State) ->
  ErServers = [connect(State) || _ <- lists:seq(1, SockCount)],
  State#state{available = ErServers, reserved = []}.

connect(#state{ip = IP, port = Port}) ->
  case er_redis:connect(IP, Port) of
    {ok, Server} -> Server;
           Other -> throw({er_pool, connect, Other})
  end.

run_error_strategy(ErError, #state{error_strategy = Strategy} = State) ->
  case Strategy of
     {wait, N} -> timer:sleep(N),
                  {noreply, State};
    {retry, N} -> case N > 0 of
                    true -> {noreply, State#state{error_strategy={retry, N-1}}};
                    false -> {stop, max_retries_reached, State}
                  end;
            _ -> {stop, {er_error, ErError}, State}
  end.

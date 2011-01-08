-module(er_pool).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, start_link/1, start_link/3, start_link/4]).

-record(state, {ip, port, available, reserved}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
  start_link(?MODULE).

start_link(GenServerName) ->
  start_link(GenServerName, "127.0.0.1", 6379).

start_link(GenServerName, IP, Port) ->
  start_link(GenServerName, IP, Port, 25).

start_link(GenServerName, IP, Port, SocketCount) ->
  gen_server:start_link({local, GenServerName}, ?MODULE,
    [IP, Port, SocketCount], []).

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
init([IP, Port, SocketCount]) when is_list(IP), is_integer(Port) ->
  process_flag(trap_exit, true),
  initial_connect(SocketCount, #state{ip = IP, port = Port}).

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
  {noreply, State#state{available = [connect(State)|T], reserved = [H | R]}};

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
  {noreply, State#state{available = [connect(State)|T], reserved = [H | R]}};

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

% An er_redis died.  Let's remove it from the proper list of servers.
handle_info({'EXIT', Pid, _Reason},
    #state{available=Available, reserved=Reserved} = State) ->
  case lists:member(Pid, Available) of
    true -> RemovedOld = Available -- [Pid],
            NewAvail = [connect(State) | RemovedOld],
            {noreply, State#state{available=NewAvail}};
    false -> RemovedOld = Reserved -- [Pid],
             NewAvail = [connect(State) | Available],
             {noreply, State#state{available = NewAvail, reserved = RemovedOld}}
  end;

handle_info(shutdown,
    #state{available=Available, reserved=Reserved} = State) ->
  [P ! shutdown || P <- Available],
  [P ! shutdown || P <- Reserved],
  {stop, normal, State#state{available=[],reserved=[]}};

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
terminate(_Reason, _State) ->
  ok.

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
  ErServers =  [connect(State) || _ <- lists:seq(1, SockCount)],
  {ok, State#state{
         available = ErServers,
         reserved = []}}.

connect(#state{ip = IP, port = Port}) ->
  case er_redis:connect(IP, Port) of
    {ok, Server} -> Server;
           Other -> throw({er_pool, connect, Other})
  end.

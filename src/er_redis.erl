-module(er_redis).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([connect/0, connect/2, q/2]).
-export([start_link/0, start_link/2]).

-record(state, {ip, port, socket}).

%%====================================================================
%% api callbacks
%%====================================================================
connect() -> start_link().
connect(IP, Port) -> start_link(IP, Port).

start_link() ->
  connect("127.0.0.1", 6379).

start_link(IP, Port) ->
  gen_server:start_link(?MODULE, [IP, Port], []).

q(Server, Parts) ->
  gen_server:call(Server, {cmd, Parts}, infinity).

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
init([IP, Port]) when is_list(IP), is_integer(Port) ->
  initial_connect(#state{ip = IP, port = Port}).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Call, From, #state{socket = undefined} = State) ->
  handle_call(Call, From, state_connect(State));

handle_call({cmd, Parts} = Cmd, From, #state{socket = Socket} = State) ->
  case gen_tcp:send(Socket, multibulk_cmd(Parts)) of
    ok -> case read_resp(Socket) of
            connection_closed -> handle_call(Cmd, From, state_connect(State));
                        Reply -> {reply, Reply, State}
          end;
    {error, closed} -> handle_call(Cmd, From, state_connect(State));
    Error -> error_logger:error_msg("SEND ERROR: ~p~n", [Error])
  end;
handle_call(next, _From, #state{socket = Socket} = State) ->
  case read_resp(Socket) of
    connection_closed -> {reply, connection_closed, state_connect(State)};
    Reply -> {reply, Reply, State}
  end;
handle_call(shutdown, _From, State) ->
  {stop, normal, State}.

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
handle_info(shutdown, State) ->
  {stop, normal, State};
handle_info(Other, State) ->
  error_logger:error_msg("Other info of: ~p~n", [Other]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket}) ->
  ok = gen_tcp:close(Socket).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_connect(State) ->
  {ok, state_connect(State)}.

state_connect(#state{ip = IP, port = Port, socket = PrevSock} = State) ->
  (catch gen_tcp:close(PrevSock)),
  SocketOpts = [binary, {nodelay, true},
                {packet, line}, {active, false},
                {recbuf, 1024}],
  case gen_tcp:connect(IP, Port, SocketOpts) of
    {ok, Socket} -> State#state{socket = Socket};
    {error, eaddrinuse} -> 
      error_logger:error_msg("No free ports.  Exiting ~p~n", [self()]),
      {stop, normal, State};
    Error ->
      error_logger:error_msg("er connect failed: ~p~n", [Error]),
      timer:sleep(380),
      State#state{socket = undefined}
  end.

strip(B) when is_binary(B) ->
  S = size(B) - 2,  % 2 = size(<<"\r\n">>)
  <<B1:S/binary, _/binary>> = B,
  B1.

read_resp(Socket) ->
  inet:setopts(Socket, [{active, once}, {packet, line}]),
  receive
    {tcp, Socket, Line} ->
      case Line of
        <<"*", Rest/binary>> ->
          Count = list_to_integer(binary_to_list(strip(Rest))),
          read_multi_bulk(Socket, Count, []);
        <<"+", Rest/binary>> ->
          strip(Rest);
        <<"-", Rest/binary>> ->
          {error, strip(Rest)};
        <<":", Size/binary>> ->
          list_to_integer(binary_to_list(strip(Size)));
        <<"$", Size/binary>> ->
          Size1 = list_to_integer(binary_to_list(strip(Size))),
          read_body(Socket, Size1);
        <<"\r\n">> ->
          read_resp(Socket);
        Uknown ->
          {unknown, Uknown}
      end;
    {tcp_closed, Socket} -> connection_closed
  end.

read_body(_Socket, -1) ->
  {ok, nil};
read_body(_Socket, 0) ->
  {ok, <<>>};
read_body(Socket, Size) ->
  inet:setopts(Socket, [{packet, raw}]),
  gen_tcp:recv(Socket, Size).

read_multi_bulk(_Data, 0, Acc) ->
  lists:reverse(Acc);
read_multi_bulk(_Data, -1, _Acc) ->
  {ok, nil};  % Occurs during b[lr]pop.  Maybe during brpoplpush too
read_multi_bulk(Socket, Count, Acc) when Count > 0 ->
  Acc1 = [read_resp(Socket) | Acc],
  read_multi_bulk(Socket, Count-1, Acc1).

-define(i2l(X), integer_to_list(X)).
multibulk_cmd(Args) when is_list(Args) ->
  ConvertedArgs = lists:flatten([to_binary(B) || B <- Args]),
  TotalLength = length(ConvertedArgs),

  ArgCount = [<<"*">>, ?i2l(TotalLength), <<"\r\n">>],
  ArgBin   = [[<<"$">>, ?i2l(iolist_size(A)), <<"\r\n">>, A, <<"\r\n">>]
               || A <- ConvertedArgs],

  [ArgCount, ArgBin];
multibulk_cmd(Args) when is_binary(Args) ->
  multibulk_cmd([Args]).

to_binary(X) when is_binary(X)  -> X;
% Basic determination of a char list: "abc"
to_binary(X) when is_list(X) andalso is_integer(hd(X)) -> list_to_binary(X);
to_binary([])                   -> <<"">>;
% Basic determination of a list of stuff: [abc, def, <<"other">>, 12]
to_binary(X) when is_list(X)    -> [to_binary(A) || A <- X];
to_binary(X) when is_atom(X)    -> atom_to_binary(X, utf8);
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X)).

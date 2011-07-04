-module(er_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helpers
-export([add_er_pool/3]).

-export([er_named_pool_spec/3]).

%% Helper macro for declaring children of supervisor
-define(ER_CHILD(I, Args, Type), {I, {er_pool, start_link, Args},
                                  permanent, 5000, Type, [er_pool]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_er_pool(Name, Host, Port) when is_atom(Name),
    is_list(Host), is_integer(Port) ->
  ChildSpec = ?ER_CHILD(Name, [Name, Host, Port], worker),
  supervisor:start_child(?MODULE, ChildSpec).

-spec er_named_pool_spec(atom(), string(), integer() | string()) -> tuple().
er_named_pool_spec(Name, IP, Port) when
     is_list(IP) andalso (is_integer(Port) orelse is_list(Port)) ->
  RedisIP   = IP,
  RedisPort = case Port of
                IntPort when is_integer(IntPort) -> IntPort;
                ListPort when is_list(ListPort) ->
                  try list_to_integer(ListPort) of
                    IntPort -> IntPort
                  catch
                    badarg:_ -> throw("port numbers must be integers!")
                  end
                end,
  {supname(Name),
   {er_pool, start_link, [Name, RedisIP, RedisPort]},
    permanent, 5000, worker, [er_pool]}.

supname(Name) ->
  list_to_atom(atom_to_list(Name) ++ "_sup").

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.

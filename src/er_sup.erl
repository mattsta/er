-module(er_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helpers
-export([add_er_pool/3]).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.

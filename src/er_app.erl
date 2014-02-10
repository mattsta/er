-module(er_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> 
  er_sup:start_link().

start(_StartType, _StartArgs) ->
  start().

stop(_State) ->
  ok.

-module(discovery_sup).

-behaviour(supervisor).

%% supervisor callbacks.
-export([init/1]).

%% discover supervisor exports.
-export([start_link/4]).

start_link(Interface, InPort, OutPort, Interval) ->
    ParamsAsList = [Interface, InPort, OutPort, Interval],
    discovery_log:debug(?MODULE, start_link, ParamsAsList),
    supervisor:start_link(?MODULE, ParamsAsList).

init([_,_,_,_] = Param) ->
    discovery_log:debug(?MODULE, init, Param),
    Childs = [child_spec(discovery_server,start_link, Param)],
    {ok, {supervisor_spec(), Childs}}.


%% ==== Private Functions

%% Supervisor strategy specification.
supervisor_spec() ->
    MaxRestart = 3,
    MaxTime = 10,
    {one_for_one, MaxRestart, MaxTime}.

%% Supervisor child specification.
child_spec(ChildModule, ChildStartFun, Args) ->
    {ChildModule,
     {ChildModule, ChildStartFun, Args},
     permanent,
     10000,
     worker,
     [ChildModule]}.




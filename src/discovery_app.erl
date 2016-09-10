-module(discovery_app).

-behaviour(application).

%% application behaviour exports
-export([start/2, stop/1]).

%% application name.
-define(APP, discovery).

-define(DEFAULT_INTERFACE, "en0").
-define(DEFAULT_INPORT   , 5000).
-define(DEFAULT_OUTPORT  , 6000). 
-define(DEFAULT_INTERVAL , 10000).

%% === Application behaviour callback functions.
start(_Type, _Args) ->
    discovery_log:debug(?MODULE, start),
    Interface    = get_env_option(interface, ?DEFAULT_INTERFACE),
    InboundPort  = get_env_option(inbound  , ?DEFAULT_INPORT),
    OutboundPort = get_env_option(outbound , ?DEFAULT_OUTPORT),
    Interval     = get_env_option(interval , ?DEFAULT_INTERVAL),

    discovery_sup:start_link(Interface, InboundPort, OutboundPort, Interval).

stop(_State) ->
    ok.

%% ==== Private functions.

%% reads application's env variables.
get_env_option(Key, DefaultValue) ->
    application:get_env(?APP, Key, DefaultValue).

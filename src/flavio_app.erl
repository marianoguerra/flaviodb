-module(flavio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/msgs/:user/:topic", handler_flavio_msgs, []}]}
    ]),
    ApiPort = 8080,
    ApiAcceptors = 100,
    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}], [
        {env, [{dispatch, Dispatch}]}
    ]),

    case flavio_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, flavio_vnode}]),

            ok = riak_core_ring_events:add_guarded_handler(flavio_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(flavio_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(flavio, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

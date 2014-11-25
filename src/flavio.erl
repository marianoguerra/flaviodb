-module(flavio).
-include("flavio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, add/2, stats/0, post_msg/3, get_msgs/4]).

-ignore_xref([ping/0, add/2, stats/0]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, flavio_vnode_master).

add(A, B) ->
    N = 3,
    W = 3,
    Timeout = 5000,

    {ok, ReqID} = flavio_op_fsm:op(N, W, {add, {A, B}}),
    wait_for_reqid(ReqID, Timeout).

post_msg(Username, Stream, Msg) ->
    N = 3,
    W = 3,
    Timeout = 5000,

    {ok, ReqID} = flavio_op_fsm:op(N, W, {post_msg, {Username, Stream, Msg}},
                                   {Username, Stream}),
    wait_for_reqid(ReqID, Timeout).

get_msgs(Username, Stream, Id, Count) ->
    N = 3,
    W = 3,
    Timeout = 5000,

    {ok, ReqID} = flavio_op_fsm:op(N, W, {get_msgs, {Username, Stream, Id, Count}},
                                   {Username, Stream}),
    wait_for_reqid(ReqID, Timeout).

stats() ->
    Timeout = 5000,
    flavio_coverage_fsm:start(stats, Timeout).

%% Private API

wait_for_reqid(ReqID, Timeout) ->
    receive {ReqID, Val} -> {ok, Val}
    after Timeout -> {error, timeout}
    end.

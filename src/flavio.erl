-module(flavio).
-include("flavio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, add/2, stats/0, post_msg/3, get_msgs/4, list_streams/1,
    list_users/0]).

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

list_streams(Username) ->
    Timeout = 5000,
    case flavio_coverage_fsm:start({list_streams, Username}, Timeout) of
        {ok, Responses} ->
            {ok, lists:filter(fun filter_empty_responses/1, Responses)};
        Other -> Other
    end.

list_users() ->
    Timeout = 5000,
    case flavio_coverage_fsm:start(list_users, Timeout) of
        {ok, Responses} ->
            {ok, lists:filter(fun filter_empty_responses/1, Responses)};
        Other -> Other
    end.

stats() ->
    Timeout = 5000,
    flavio_coverage_fsm:start(stats, Timeout).

%% Private API

wait_for_reqid(ReqID, Timeout) ->
    receive {ReqID, Val} -> {ok, Val}
    after Timeout -> {error, timeout}
    end.

filter_empty_responses({_Partition, _Node, {ok, []}}) -> false;
filter_empty_responses({_Partition, _Node, _Streams}) -> true.

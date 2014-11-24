-module(flavio).
-include("flavio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, add/2]).

-ignore_xref([ping/0, add/2]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, flavio_vnode_master).

add(A, B) ->
    DocIdx = riak_core_util:chash_key({<<"add">>, term_to_binary({A, B})}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {add, A, B}, flavio_vnode_master).

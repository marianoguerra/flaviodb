-module(flavio_coverage_fsm).

-export([start_link/4, start/2]).
-export([init/2, process_results/2, finish/2]).

-behaviour(riak_core_coverage_fsm).

-record(state, {req_id, from, request, accum=[]}).

%% API

start_link(ReqId, From, Request, Timeout) ->
    riak_core_coverage_fsm:start_link(?MODULE, {pid, ReqId, From}, [ReqId, From, Request, Timeout]).

start(Request, Timeout) ->
    ReqId = reqid(),
    {ok, _} = flavio_coverage_fsm_sup:start_fsm([ReqId, self(), Request, Timeout]),
    receive
        {ReqId, Val} -> Val
    end.

%% riak_core_coverage_fsm API

init(_, [ReqId, From, Request, Timeout]) ->
    State = #state{req_id=ReqId, from=From, request=Request},
    {Request, allup, 1, 1, flavio, flavio_vnode_master, Timeout, State}.

process_results({{_ReqId, {Partition, Node}}, Data}, State=#state{accum=Accum}) ->
    NewAccum = [{Partition, Node, Data}|Accum],
    {done, State#state{accum=NewAccum}}.

finish(clean, S=#state{req_id=ReqId, from=From, accum=Accum}) ->
    From ! {ReqId, {ok, Accum}},
    {stop, normal, S};

finish({error, Reason}, S=#state{req_id=ReqId, from=From, accum=Accum}) ->
    lager:warning("Coverage query failed! Reason: ~p", [Reason]),
    From ! {ReqId, {partial, Reason, Accum}},
    {stop, normal, S}.

%% Private API

reqid() -> erlang:phash2(erlang:now()).

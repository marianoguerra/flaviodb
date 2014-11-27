-module(handler_flavio_msgs).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         resource_exists/2,
         from_json/2,
         to_json/2
        ]).

-record(state, {username, topic, from, limit}).

to_int_or(Bin, Default) ->
    Str = binary_to_list(Bin),
    case string:to_integer(Str) of
        {Int, ""} -> Int;
        _ -> Default
    end.

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {Username, Req1} = cowboy_req:binding(username, Req),
    {Topic, Req2} = cowboy_req:binding(topic, Req1),

    {FromStr, Req3} = cowboy_req:qs_val(<<"from">>, Req2, <<"">>),
    {LimitStr, Req4} = cowboy_req:qs_val(<<"limit">>, Req3, <<"1">>),

    From = to_int_or(FromStr, nil),
    Limit = to_int_or(LimitStr, 1),

	{ok, Req4, #state{username=Username, topic=Topic, from=From, limit=Limit}}.

allowed_methods(Req, State) -> {[<<"POST">>, <<"GET">>], Req, State}.

resource_exists(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    Exists = case Method of
                 <<"POST">> -> false;
                 _ -> true
             end,
    {Exists, Req1, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

from_json(Req, State=#state{username=Username, topic=Topic}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case jsx:is_json(Body) of
        true ->
            Data = jsx:decode(Body),
            Msg = proplists:get_value(<<"msg">>, Data, nil),

            if is_binary(Msg) ->
                   {ok, [FirstResponse|_]} = flavio:post_msg(Username, Topic, Msg),
                   {{ok, Entity}, _Partition} = FirstResponse,
                   EntityPList = fixstt:to_proplist(Entity),
                   EntityJson = jsx:encode(EntityPList),
                   response(Req, State, EntityJson);
               true ->
                   bad_request(Req1, State, <<"{\"type\": \"no-msg\"}">>)
            end;
        false ->
            bad_request(Req1, State, <<"{\"type\": \"invalid-body\"}">>)
    end.

to_json(Req, State=#state{username=Username, topic=Topic, from=From, limit=Limit}) ->
    {ok, [FirstResponse|_]} = flavio:get_msgs(Username, Topic, From, Limit),
    {{ok, Entities}, _Partition} = FirstResponse,
    EntitiesPList = lists:map(fun fixstt:to_proplist/1, Entities),
    EntitiesJson = jsx:encode(EntitiesPList),

    {EntitiesJson, Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

%% Private
bad_request(Req, State, Body) ->
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {false, Req1, State}.

response(Req, State, Body) ->
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {true, Req1, State}.


-module(handler_flavio_msgs).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         resource_exists/2,
         from_json/2
        ]).

-record(state, {username, topic}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {Username, Req1} = cowboy_req:binding(username, Req),
    {Topic, Req2} = cowboy_req:binding(topic, Req1),

	{ok, Req2, #state{username=Username, topic=Topic}}.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    Exists = true,
    {Exists, Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

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


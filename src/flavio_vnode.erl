-module(flavio_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("flavio.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([start_vnode/1]).

-record(state, {partition, ops_count=0, base_dir}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition=Partition, base_dir="flavio_data" }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({RefId, {add, {A, B}}}, _Sender, State=#state{ops_count=CurrentCount}) ->
    NewCount = CurrentCount + 1,
    NewState = State#state{ops_count=NewCount},
    {reply, {RefId, {A + B, State#state.partition}}, NewState};

handle_command({RefId, {post_msg, {Username, Stream, Msg}}}, _Sender, State) ->
    Entry = fixstt:new(Msg),
    Result = post_msg(State, Username, Stream, Entry),
    {reply, {RefId, {Result, State#state.partition}}, State};

handle_command({RefId, {get_msgs, {Username, Stream, Id, Count}}}, _Sender, State) ->
    {ok, StreamIo} = get_stream(State, Username, Stream),
    Result = case fixsttio:read(StreamIo, Id, Count) of
                 {ok, StreamIo1, Entries} ->
                     {ok, _StreamIo2} = fixsttio:close(StreamIo1),
                     {ok, Entries};
                 Other -> Other
             end,
    {reply, {RefId, {Result, State#state.partition}}, State};

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State=#state{partition=Partition}) ->
    lager:info("fold req ~p", [Partition]),
    AllPairs = list_all(State),
    HandlePair = fun (Key={Username, StreamName}, AccIn) ->
                         lager:info("handling handoff for ~s/~s", [Username, StreamName]),
                         HandleEntry = fun (Entry, AccIn0) ->
                                               AccIn1 = Fun(Key, Entry, AccIn0),
                                               {continue, AccIn1}
                                       end,

                         StreamPath = stream_path(State, Username, StreamName),
                         {ok, FixSttIo} = fixsttio:open(StreamPath),
                         {ok, AccIn1} = fixsttio:iterate(FixSttIo, HandleEntry, AccIn),
                         {ok, _ClosedFixSttIo} = fixsttio:close(FixSttIo),

                         AccIn1
                 end,

    AccFinal = lists:foldl(HandlePair, Acc0, AllPairs),

    {reply, AccFinal, State};

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff starting ~p", [Partition]),
    {true, State}.

handoff_cancelled(State=#state{partition=Partition}) ->
    lager:info("handoff cancelled ~p", [Partition]),
    {ok, State}.

handoff_finished(_TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff finished ~p", [Partition]),
    {ok, State}.

handle_handoff_data(BinData, State) ->
    TermData = binary_to_term(BinData),
    lager:debug("handoff data received ~p", [TermData]),
    {{Username, StreamName}, Entry} = TermData,
    Result = case post_msg(State, Username, StreamName, Entry) of
                {ok, _WrittenEntry}-> ok;
                {error, _Reason}= Other -> Other
            end,
    {reply, Result, State}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State=#state{partition=Partition}) ->
    Path = partition_path(State),
    IsEmpty = ((not filelib:is_dir(Path)) orelse
               (length(list_users(State)) == 0)),
    lager:info("handoff is empty? ~p ~p", [IsEmpty, Partition]),
    {IsEmpty, State}.

delete(State) ->
    Path = partition_path(State),
    lager:info("handoff delete ~s", [Path]),
    remove_path(Path),
    {ok, State}.

handle_coverage(stats, _KeySpaces, {_, RefId, _}, State=#state{ops_count=OpsCount}) ->
    {reply, {RefId, [{ops_count, OpsCount}]}, State};

handle_coverage({list_streams, Username}, _KeySpaces, {_, RefId, _}, State) ->
    Streams = lists:sort(list_streams(State, Username)),
    {reply, {RefId, {ok, Streams}}, State};

handle_coverage(list_users, _KeySpaces, {_, RefId, _}, State) ->
    Users = lists:sort(list_users(State)),
    {reply, {RefId, {ok, Users}}, State};

handle_coverage(Req, _KeySpaces, _Sender, State) ->
    lager:warning("unknown coverage received ~p", [Req]),
    {noreply, State}.

handle_exit(_Pid, _Reason, State=#state{partition=Partition}) ->
    lager:info("handle exit ~p", [Partition]),
    {noreply, State}.

terminate(Reason, #state{partition=Partition}) ->
    lager:info("terminate ~p: ~p", [Partition, Reason]),
    ok.

%% Private API

list_dir(Path) ->
    case file:list_dir(Path) of
        {error, enoent} -> [];
        {ok, Names} -> Names
    end.

list_users(State) ->
    Path = partition_path(State),
    lists:map(fun list_to_binary/1, list_dir(Path)).

list_streams(#state{partition=Partition, base_dir=BaseDir}, Username) ->
    PartitionStr = integer_to_list(Partition),
    UserPath = filename:join([BaseDir, PartitionStr, Username]),
    lists:map(fun list_to_binary/1, list_dir(UserPath)).

list_all(State) ->
    Users = list_users(State),
    lists:flatmap(fun (Username) ->
                          UserStreams = list_streams(State, Username),
                          lists:map(fun (StreamName) ->
                                            {Username, StreamName}
                                    end, UserStreams)
                  end, Users).

stream_path(#state{partition=Partition, base_dir=BaseDir}, Username, Stream) ->
    PartitionStr = integer_to_list(Partition),
    filename:join([BaseDir, PartitionStr, Username, Stream, "msgs"]).

get_stream(State, Username, Stream) ->
    StreamPath = stream_path(State, Username, Stream),
    ok = filelib:ensure_dir(StreamPath),
    fixsttio:open(StreamPath).

partition_path(#state{partition=Partition, base_dir=BaseDir}) ->
    PartitionStr = integer_to_list(Partition),
    filename:join([BaseDir, PartitionStr]).

% sub_file and remove_recursive adapted from
% https://github.com/erlware/erlware_commons/blob/master/src/ec_file.erl
sub_files(From) ->
    {ok, SubFiles} = file:list_dir(From),
    [filename:join(From, SubFile) || SubFile <- SubFiles].

remove_path(Path) ->
    case filelib:is_dir(Path) of
        false ->
            file:delete(Path);
        true ->
            lists:foreach(fun(ChildPath) ->
                                  remove_path(ChildPath)
                          end, sub_files(Path)),
            file:del_dir(Path)
    end.

post_msg(State, Username, Stream, Entry) ->
    {ok, StreamIo} = get_stream(State, Username, Stream),
    case fixsttio:append(StreamIo, Entry) of
        {ok, StreamIo1, EntryId} ->
            {ok, _StreamIo2} = fixsttio:close(StreamIo1),
            {ok, fixstt:set(Entry, id, EntryId)};
        Other -> Other
    end.

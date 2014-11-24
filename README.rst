Flaviodb
========

setup rebar riak_core template
------------------------------

::

    git clone https://github.com/basho/rebar_riak_core.git
    cd rebar_riak_core
    make install

create project template
-----------------------

::

    mkdir flaviodb
    cd flaviodb

    # download rebar and set executable permissions
    wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar

    # create project from riak_core template with app id set to flavio
    ./rebar create template=riak_core appid=flavio

update riak_core version to 2.0.0
---------------------------------

we change the version on deps, riak_core and lager here:

https://github.com/marianoguerra/flaviodb/commit/6ae8f8b83c3d15346ea35a586dbe624aa3de6967

note we also updated lager version to 2.0.3 and removed the warnings_as_errors
flag on the last line.

trying to build it
------------------

::

    make rel

if you are using Erlang 17 you will get some errors while compiling the
dependencies, the way to fix them most of the time is pretty ugly but I don't
know a better way.

the solution is to remove the warnings_as_errors config flag on rebar.config
directly on the rebar.config for that dependency, I did a script you can run
to fix this, run it once after you fetched the dependencies::

    ./util/fix_deps_warnings_as_errors.sh

if you ran the command above try building again::


    make rel

success!

now what?

running a node
--------------

now that we managed to build it let's start a node::

    ./rel/flavio/bin/flavio console

but what can we do with it? well we can ping it::


    (flavio@127.0.0.1)1> flavio:ping().
    {pong,1210306043414653979137426502093171875652569137152}

now you have a distributed, scalable and fault-tolerant ping service!

the road of the ping
--------------------

now that we have the basic riak_core project running let's follow the ping on
it's way from call to response.

it's entry point and public api is the flavio module, that means we have to
look into flavio.erl::

    -module(flavio).
    -include("flavio.hrl").
    -include_lib("riak_core/include/riak_core_vnode.hrl").

    -export([ping/0]).

    -ignore_xref([ping/0]).

    %% Public API

    %% @doc Pings a random vnode to make sure communication is functional
    ping() ->
        DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
        PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio),
        [{IndexNode, _Type}] = PrefList,
        riak_core_vnode_master:sync_spawn_command(IndexNode, ping, flavio_vnode_master).

we see we have our ping function there as the only public API and it does some
funny stuff.

I won't go into much riak_core details that are described elsewhere since this
is a talk that covers the practical aspects, there are many useful talks about
riak_core internals and theory around, you can watch them:

* http://vimeo.com/21772889
* http://vimeo.com/18758206

there are also some detailed articles about it:

* https://github.com/rzezeski/try-try-try
* https://github.com/basho/riak_core/wiki
* http://basho.com/where-to-start-with-riak-core/

but let's look at what it does line by line::

        DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),

the line above hashes a key to decide to which vnode the call should go, a
riak_core app has a fixed number of vnodes that are distributed across all the
instances of your app, vnodes move from instance to instance when the number of
instances change to balance the load and have fault tolerance and scalability.

The call above will allow us to ask for vnodes that can handle that hashed key,
let's run it in the app console to see what it does::

    (flavio@127.0.0.1)1> DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}).
    <<207,185,91,89,64,167,168,83,113,154,212,211,27,36,113, 251,56,179,28,123>>

we seem to get a binary back, in the next line we ask for a list of vnodes that
can handle that hashed key::

        PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio),

let's run it to see what it does::

    (flavio@127.0.0.1)2> PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio).
    [{{1187470080331358621040493926581979953470445191168,
    'flavio@127.0.0.1'},
    primary}]

we get a list with one tuple that has 3 items, a long number, something that looks like a hist
and an atom, let's try changing the number 1::

    (flavio@127.0.0.1)4> PrefList2 = riak_core_apl:get_primary_apl(DocIdx, 2, flavio).
    [{{1187470080331358621040493926581979953470445191168,
       'flavio@127.0.0.1'},
      primary},
     {{1210306043414653979137426502093171875652569137152,
       'flavio@127.0.0.1'},
      primary}]

now we get two tuples, the first one is the same, so what this does is to return
the number of vnodes that can handle the request from the hashed key by priority.

btw, the first number is the vnode id, it's what we get on the ping response :)

next line just unpacks the pref list to get the vnode id and ignore the other part::

        [{IndexNode, _Type}] = PrefList,

and finally we ask riak_core to call the ping command on the IndexNode we got back::

        riak_core_vnode_master:sync_spawn_command(IndexNode, ping, flavio_vnode_master).

let's try it on the console::

    (flavio@127.0.0.1)5> [{IndexNode, _Type}] = PrefList.
    [{{1187470080331358621040493926581979953470445191168, 'flavio@127.0.0.1'}, primary}]

    (flavio@127.0.0.1)6> riak_core_vnode_master:sync_spawn_command(IndexNode, ping, flavio_vnode_master).
    {pong,1187470080331358621040493926581979953470445191168}

you can see we get IndexNode back in the pong response, now let's try passing the second IndexNode::

    (flavio@127.0.0.1)7> [{IndexNode1, _Type1}, {IndexNode2, _Type2}] = PrefList2.
    [{{1187470080331358621040493926581979953470445191168, 'flavio@127.0.0.1'}, primary},
     {{1210306043414653979137426502093171875652569137152, 'flavio@127.0.0.1'}, primary}]

    (flavio@127.0.0.1)8> riak_core_vnode_master:sync_spawn_command(IndexNode2, ping, flavio_vnode_master).
    {pong,1210306043414653979137426502093171875652569137152}

we get the IndexNode2 back, that means that the request was sent to the second
vnode instead of the first one.

but where does the command go? the road is explained in this scientific chart::

    flavio.erl -> riak_core magic -> flavio_vnode.erl

let's see the content of flavio_vnode.erl (just the useful parts)::

    -module(flavio_vnode).
    -behaviour(riak_core_vnode).

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

    -record(state, {partition}).

    %% API
    start_vnode(I) ->
        riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

    init([Partition]) ->
        {ok, #state { partition=Partition }}.

    %% Sample command: respond to a ping
    handle_command(ping, _Sender, State) ->
        {reply, {pong, State#state.partition}, State};
    handle_command(Message, _Sender, State) ->
        ?PRINT({unhandled_command, Message}),
        {noreply, State}.

ok, let's go by parts, first we declare our module::

    -module(flavio_vnode).

then we specify that we want to implement the riak_core_vnode behaviour::

    -behaviour(riak_core_vnode).

behaviours in erlang are like interfaces, a set of functions that a module must
implement to satisfy the behaviour specification, you can read more here:

http://www.erlang.org/doc/design_principles/des_princ.html

in this case riak_core defines a behaviour with a set of functions we must
implement to be a valid riak_core vnode, you can get an idea of the kind of
functionality we need by looking at the exported functions::

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

for the moment most of them have a "dummy" implementation where they just to
the minimal amount of work to satisfy the behaviour and not more, it's our job
to change the default implementation to fit our needs.

we will have a record called state to keep info between callbacks, this is
typical erlang way of managing state so I won't cover it here::

    -record(state, {partition}).

then we implement the api to start the vnode, nothing fancy::

    %% API
    start_vnode(I) ->
        riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

note that on init we store the Partition value on state so we can use it later,
this is what I referred above as vnode id, it's the big number you saw before::

    init([Partition]) ->
        {ok, #state { partition=Partition }}.

and now for the interesting part, here we have our ping command implementation,
we match for ping in the Message position (the first argument)::
    handle_command(ping, _Sender, State) ->

and return a reply response with the second item in the tuple being the actual
response that the caller will get where we reply with the atom pong and the
partition number of this vnode, the last item in the tuple is the new state we
want to have for this vnode, since we didn't change anything we pass the
current value::

        {reply, {pong, State#state.partition}, State};

and then we implement a catch all that will just print the unknown command and
give no reply back::

    handle_command(Message, _Sender, State) ->
        ?PRINT({unhandled_command, Message}),
        {noreply, State}.

so this is the roundtrip of the ping call, our task to add more commands will be:

* add a function on flavio.erl that hides the internal work done to distribute the work
* add a new match on handle_command to match the command we added on flavio.erl and provide a reply

but before adding a new command let's play with the distribution part of
riak_core.

in our case we have all the vnodes on the same instance and on the same machine
so that's not that distributed, let's try running more than one node.

creating a local cluster
------------------------

to create a local cluster we will need to create and start N different builds
and instances with slightly different configurations given the fact that all
instances are running on the same machine and share the same resources.

you can read more about devrel here:

https://github.com/basho/rebar_riak_core#devrel

first stop your running instance if you still have it running, then run::

    make devrel

you can see at the end of the output something similar to this::

    mkdir -p dev
    rel/gen_dev dev1 rel/vars/dev_vars.config.src rel/vars/dev1_vars.config
    Generating dev1 - node='flavio1@127.0.0.1' http=10018 handoff=10019
    (cd rel && /home/mariano/src/rct/flaviodb/rebar generate target_dir=../dev/dev1 overlay_vars=vars/dev1_vars.config)
    ==> rel (generate)
    mkdir -p dev
    rel/gen_dev dev2 rel/vars/dev_vars.config.src rel/vars/dev2_vars.config
    Generating dev2 - node='flavio2@127.0.0.1' http=10028 handoff=10029
    (cd rel && /home/mariano/src/rct/flaviodb/rebar generate target_dir=../dev/dev2 overlay_vars=vars/dev2_vars.config)
    ==> rel (generate)
    mkdir -p dev
    rel/gen_dev dev3 rel/vars/dev_vars.config.src rel/vars/dev3_vars.config
    Generating dev3 - node='flavio3@127.0.0.1' http=10038 handoff=10039
    (cd rel && /home/mariano/src/rct/flaviodb/rebar generate target_dir=../dev/dev3 overlay_vars=vars/dev3_vars.config)
    ==> rel (generate)
    mkdir -p dev
    rel/gen_dev dev4 rel/vars/dev_vars.config.src rel/vars/dev4_vars.config
    Generating dev4 - node='flavio4@127.0.0.1' http=10048 handoff=10049
    (cd rel && /home/mariano/src/rct/flaviodb/rebar generate target_dir=../dev/dev4 overlay_vars=vars/dev4_vars.config)

you can see it generated 4 builds (dev1 ... dev4) and that it assigned different names
(flavio1 ... flavio4) and assigned different ports for http and handoff.

now let's start them::

    for d in dev/dev*; do $d/bin/flavio start; done

now instead of starting and connecting to a console as before we just started
the nodes, but how do we know they are running?

welp, we can ping them from the command line tool that the template kindly provides to us::

    for d in dev/dev*; do $d/bin/flavio ping; done

we should see 4 individual pong replies::

    pong
    pong
    pong
    pong

but we don't have a cluster yet, because each instance is running unaware of the others, to make them
an actual cluster we have to make them aware of each other.

you can see that they aren't aware by asking any of them about the status of its members::

    $ dev/dev1/bin/flavio-admin member-status

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid     100.0%      --      'flavio1@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

you see flavio1 "cluster" has only one node in it (itself), try with another::

    $ dev/dev4/bin/flavio-admin member-status

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid     100.0%      --      'flavio4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

note dev4 instead of dev1 in the command.

now we will ask nodes 2, 3 and 4 to join node 1 on a cluster::

    $ for d in dev/dev{2,3,4}; do $d/bin/flavio-admin cluster join flavio1@127.0.0.1; done

    Success: staged join request for 'flavio2@127.0.0.1' to 'flavio1@127.0.0.1'
    Success: staged join request for 'flavio3@127.0.0.1' to 'flavio1@127.0.0.1'
    Success: staged join request for 'flavio4@127.0.0.1' to 'flavio1@127.0.0.1'

check again the cluster status::

    $ dev/dev1/bin/flavio-admin member-status

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    joining     0.0%      --      'flavio2@127.0.0.1'
    joining     0.0%      --      'flavio3@127.0.0.1'
    joining     0.0%      --      'flavio4@127.0.0.1'
    valid     100.0%      --      'flavio1@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:3 / Down:0dev/dev1/bin/flavio-admin member-status

they are joining, because we have to approve cluster changes, let's look what's
the plan::

    $ dev/dev1/bin/flavio-admin cluster plan

    =============================== Staged Changes ================================
    Action         Details(s)
    -------------------------------------------------------------------------------
    join           'flavio2@127.0.0.1'
    join           'flavio3@127.0.0.1'
    join           'flavio4@127.0.0.1'
    -------------------------------------------------------------------------------


    NOTE: Applying these changes will result in 1 cluster transition

    ###############################################################################
                             After cluster transition 1/1
    ###############################################################################

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid     100.0%     25.0%    'flavio1@127.0.0.1'
    valid       0.0%     25.0%    'flavio2@127.0.0.1'
    valid       0.0%     25.0%    'flavio3@127.0.0.1'
    valid       0.0%     25.0%    'flavio4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

    Transfers resulting from cluster changes: 48
      16 transfers from 'flavio1@127.0.0.1' to 'flavio4@127.0.0.1'
      16 transfers from 'flavio1@127.0.0.1' to 'flavio3@127.0.0.1'
      16 transfers from 'flavio1@127.0.0.1' to 'flavio2@127.0.0.1'

looks good to me, let's commit that plan so it actually happens::

    $ dev/dev1/bin/flavio-admin cluster commit

    Cluster changes committed

let's see the cluster status again::

    $ dev/dev1/bin/flavio-admin member-status

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      25.0%      --      'flavio1@127.0.0.1'
    valid      25.0%      --      'flavio2@127.0.0.1'
    valid      25.0%      --      'flavio3@127.0.0.1'
    valid      25.0%      --      'flavio4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0


now the cluster has 4 nodes which have the ring distributed equally :)

to just be sure it's not all a lie, let's connect to some nodes and run the
ping again, first from node 1::

    $ dev/dev1/bin/flavio attach
    Attaching to /tmp//home/mariano/src/rct/flaviodb/dev/dev1/erlang.pipe.1 (^D to exit)

    (flavio1@127.0.0.1)1> flavio:ping().
    {pong,822094670998632891489572718402909198556462055424}
    (flavio1@127.0.0.1)2> [Quit]

now from node 3::

    $ dev/dev3/bin/flavio attach
    Attaching to /tmp//home/mariano/src/rct/flaviodb/dev/dev3/erlang.pipe.1 (^D to exit)

    (flavio3@127.0.0.1)1> flavio:ping()
    (flavio3@127.0.0.1)1> .
    {pong,1438665674247607560106752257205091097473808596992}
    (flavio3@127.0.0.1)2> [Quit]

note that we got the reply from a different vnode the second time.

adding a command
----------------

first let's add a simple command to get the workflow right.

we will build a calculation command first and then we will add some state
tracking to it.

our command will start simply by adding two numbers and returning the result
and the vnode that calculated the result.

let's start by defining our new command from the user's perspective, we want to
be able to run::

    flavio:add(2, 5).

and get our result back, so let's add the add function to the flavio module,
first we add it to the list of the exported functions::

    -export([ping/0, add/2]).

and then we add our implementation starting from the ping version::

    add(A, B) ->
        DocIdx = riak_core_util:chash_key({<<"add">>, term_to_binary({A, B})}),
        PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio),
        [{IndexNode, _Type}] = PrefList,
        riak_core_vnode_master:sync_spawn_command(IndexNode, {add, A, B}, flavio_vnode_master).

the changes are, the name (of course), the parameters it accepts, in our case it accepts two numbers,
but more subtle changes are in the following line::

        DocIdx = riak_core_util:chash_key({<<"add">>, term_to_binary({A, B})}),

we change the name of the command (the first item in the tuple) and we also
changed the content of the arguments to term_to_binary, we could leave now()
there so the call will generate a new number on each call producing a different hash and therefore routing to a different vnode each time, but in our case we want a little more predictability.

we will pass the numbers we want to add as the second item in the tuple, this
means that if we want to add the same two numbers we will be routed to the same
vnodes every time, this is part of the "consistent hashing" you may have heard
about riak_core, we will try it in action later, but for now let's move to the next lines.

this two stay the same::

        PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, flavio),
        [{IndexNode, _Type}] = PrefList,

but the last one changed slightly::

        riak_core_vnode_master:sync_spawn_command(IndexNode, {add, A, B}, flavio_vnode_master).

instead of passing ping as second parameter we pass our "command", that is,
which operation we want to perform and the parameters, this may seem familiar
if you ever implemented something like gen_server, if not, we basically send a message
with the information of the command we want to call and the other side matches
the message with the commands it understands and acts accordingly.

in our case now we must match this message/command on the vnode implementation,
this should be really easy, on flavio_vnode.erl we add the following clause to
the existing handle_command function::

    handle_command({add, A, B}, _Sender, State) ->
        {reply, {A + B, State#state.partition}, State};

you can see we match the command on the first argument and as reply on the
second position of the tuple we send the response back, which contains the
addition as first item and the partition on as seconds, this just to keep track
of the routing, it's not needed to return it.

now stop your current instance if you have one running and build a new release::

    rm -rf rel/flavio
    make rel


now let's play a little with it::

    $ ./rel/flavio/bin/flavio console

    (flavio@127.0.0.1)1> flavio:add(2, 5).
    {7,959110449498405040071168171470060731649205731328}

    (flavio@127.0.0.1)2> flavio:add(2, 5).
    {7,959110449498405040071168171470060731649205731328}

    (flavio@127.0.0.1)3> flavio:add(2, 5).
    {7,959110449498405040071168171470060731649205731328}

    (flavio@127.0.0.1)4> flavio:add(3, 5).
    {8,91343852333181432387730302044767688728495783936}

    (flavio@127.0.0.1)5> flavio:add(3, 5).
    {8,91343852333181432387730302044767688728495783936}

    (flavio@127.0.0.1)6> flavio:add(2, 5).
    {7,959110449498405040071168171470060731649205731328}

    (flavio@127.0.0.1)7> flavio:add(2, 9).
    {11,1255977969581244695331291653115555720016817029120}

    (flavio@127.0.0.1)8> flavio:add(2, 9).
    {11,1255977969581244695331291653115555720016817029120}

    (flavio@127.0.0.1)9> flavio:add(2, 5).
    {7,959110449498405040071168171470060731649205731328}

you can see that the same addition gets sent to the same vnode each time, if
the parameters change then it's sent to another one, but consistently.

this is how we handle scaling and distribution, by deciding which information
of our command is part of the hash key, this varies with each problem so it's
a design decision you have to make.

the full change is here: https://github.com/marianoguerra/flaviodb/commit/8e0fb2460791651fcc1aa5cd957b535437d07095

keeping some state
------------------

this operations are stateless so it doesn't make much sense to route them
consistently, but now we will add some state tracking to count how many
additions each vnode made.

for this we will increment a operations counter on each vnode when an operation
is made and we will provide a way to retrieve this information as another
command.

first let's start by adding a new field to our state record to keep the count::

    -record(state, {partition, ops_count=0}).

and then when we receive an addition command we increment the count and return
the new state in the 3 item tuple so that this new state becomes the vnode
state::

    handle_command({add, A, B}, _Sender, State=#state{ops_count=CurrentCount}) ->
        NewCount = CurrentCount + 1,
        NewState = State#state{ops_count=NewCount},
        {reply, {A + B, State#state.partition}, NewState};

line by line, first we match the current ops_count::

    handle_command({add, A, B}, _Sender, State=#state{ops_count=CurrentCount}) ->

then calculate the new count::

        NewCount = CurrentCount + 1,

then create the new state record that is the same as the old one but with the
new count::

        NewState = State#state{ops_count=NewCount},

and then we reply as before but we pass NewState as third item::

        {reply, {A + B, State#state.partition}, NewState};

rebuild and run::

    $ rm -rf rel/flavio && make rel && ./rel/flavio/bin/flavio console

    (flavio@127.0.0.1)1> flavio:add(2, 5).
    {7,959110449498405040071168171470060731649205731328}
    (flavio@127.0.0.1)2> flavio:add(2, 6).
    {8,1278813932664540053428224228626747642198940975104}

the full change is here: https://github.com/marianoguerra/flaviodb/commit/3b8a789308767f735ce45590f4d1887e2dbdb1b4

nothing different because we need a way to get that count, for that we will
implement a new command, get_stats, but how do we tell to which vnode?
can we ask all vnodes for this info?

well yes we can, it's called a coverage call, and it's a call that involves all
the vnodes

first we add the stats function to the export list::

    -export([ping/0, add/2, stats/0]).

now we add the implementation::

    stats() ->
        Timeout = 5000,
        flavio_coverage_fsm:start(stats, Timeout).

well, that was easy... but what is this flavio_coverage_fsm:start thing?

the high level description of a coverage call is that we do a coverage call for
all the vnodes and collect the results until we have all of them or until
timeout happens, this coverage call is implemented in the vnode by adding a clause
on the handle_coverage function to match the command sent to it, in our case,
we pass the atom "stats".

but someone has to take care of making the calls to all the vnodes, accumulating
the results and timing out if necessary.

for that riak_core provides a behaviour called riak_core_coverage_fsm, which
provides some callbacks we must implement and everything else will be handled
by riak_core, the callbacks we must implement are needed to init the state of
the process, to process each individual result and to do an action when the
collection is finished.

for the most basic case we will just initialize with some configured values,
init the state, on each individual result we will accumulate it and maybe
summarize it in some way and on finalization we return the result, we may also
do some summarization or cleanup if needed.

the code of flavio_coverage_fsm and flavio_coverage_fsm_sup (it's supervisor)
are really straight forward if you ever implemented something like a gen_fsm,
if not you can live by copying and pasting it and tweaking some details but at
some point you should go over and read about gen_fsm and OTP in general to get
a better sense of what's happening there.

but before we go to the vnode implementation other than creating this two new
modules to help us with our coverage call we need to register this new supervisor
in the our supervisor tree, this is also an OTP thing that you should investigate
on your own, there's a lot of useful information about it on the Erlang docs, books
and in Learn You Some Erlang.

to add this supervisor to the supervisor tree we must edit the file
flavio_sup.erl and add the following::

    init(_Args) ->
        VMaster = { flavio_vnode_master,
                      {riak_core_vnode_master, start_link, [flavio_vnode]},
                      permanent, 5000, worker, [riak_core_vnode_master]},

        CoverageFSMs = {flavio_coverage_fsm_sup,
                        {flavio_coverage_fsm_sup, start_link, []},
                        permanent, infinity, supervisor, [flavio_coverage_fsm_sup]},
        {ok,
            { {one_for_one, 5, 10},
              [VMaster, CoverageFSMs]}}.

we added the CoverageFSMs definition and we added it to the list on the last
line.

the part that's interesting to us is the api call and the callback that must be
implemented in the vnode, which goes as follows::

    handle_coverage(stats, _KeySpaces, {_, RefId, _}, State=#state{ops_count=OpsCount}) ->
        {reply, {RefId, [{ops_count, OpsCount}]}, State};
    handle_coverage(Req, _KeySpaces, _Sender, State) ->
        lager:warning("unknown coverage received ~p", [Req]),
        {norepl, State}.

we redefine the whole handle_coverage function to avoid it from stopping the
vnode in case it gets a coverage call it doesn't know about and change it so
that it only logs a warning and ignores it.

but the interesting function clause is the first one where we match the RefId
that is passed to us from flavio_coverage_fsm, which uses it to differentiate
all the calls and we also get from our state the info we are going to reply.

we reply with a two item tuple where the first item is the RefId we got and the
second is the coverage call response.

in this case I return a `proplist <http://www.erlang.org/doc/man/proplists.html>`_ just
to future proof this call and allow to return more information in the future.

now we rebuild and run the release to play with it::

    $ rm -rf rel/flavio && make rel && ./rel/flavio/bin/flavio console

    (flavio@127.0.0.1)1> flavio:stats().
    {ok,[ lot of output here]}

    % let's use the api a little

    (flavio@127.0.0.1)2> flavio:add(2, 5).
    {7,959110449498405040071168171470060731649205731328}
    (flavio@127.0.0.1)3> flavio:add(2, 6).
    {8,1278813932664540053428224228626747642198940975104}
    (flavio@127.0.0.1)4> flavio:add(2, 6).
    {8,1278813932664540053428224228626747642198940975104}
    (flavio@127.0.0.1)5> flavio:add(2, 6).
    {8,1278813932664540053428224228626747642198940975104}
    (flavio@127.0.0.1)6> flavio:add(3, 6).
    {9,182687704666362864775460604089535377456991567872}
    (flavio@127.0.0.1)7> flavio:add(3, 6).
    {9,182687704666362864775460604089535377456991567872}
    (flavio@127.0.0.1)8> flavio:stats().
    {ok,[ lot of output here, maybe you can see some with ops_count > 0]}

    % let's filter the output to see interesting info

    (flavio@127.0.0.1)9> {ok, Stats} = flavio:stats().
    {ok,[ again lot of output here]}

    (flavio@127.0.0.1)10> lists:filter(fun ({_, _, [{ops_count, OpsCount}]}) -> OpsCount > 0 end, Stats).
    [{1278813932664540053428224228626747642198940975104, 'flavio@127.0.0.1', [{ops_count,3}]},
     {959110449498405040071168171470060731649205731328, 'flavio@127.0.0.1', [{ops_count,1}]},
     {182687704666362864775460604089535377456991567872, 'flavio@127.0.0.1', [{ops_count,2}]}]

we can see in the last call that there are 3 nodes that have ops_count set to
a value bigger than 0 and that matches the calls we did above.

the full change is here: https://github.com/marianoguerra/flaviodb/commit/9b6ef0ea2b9f0257733024b1468016a5d96b713c

tolerating faults in our additions (?)
--------------------------------------

you know computers cannot be trusted, so we may want to run our commands in
more than one vnode and wait for a subset (or all of them) to finish before
considering the operation to be successful, for this when a command is ran we
will send the command to a number of vnodes, let's call it W and wait for a
number of them to succeed, let's call it N.

to do this we will need to do something similar than what we did with coverage
calls, we will need to setup a process that will send the command to a number
of vnodes and accumulate the responses or timeout if it takes to long, then
send the result back to the caller. We will also need a supervisor for it and
to register this supervisor in our main supervisor tree.

again I won't go into details on the fsm and supervisor implementations, maybe
I will add an annex later or comment the code heavily in case you want to
understand how it works, but just for you to know, I tend to copy those fsms from
other projects and adapt them to my needs, just don't tell anybody ;)

the code for the "caller/accumulator/waiter/replier" is in
flavio_io_fsm_sup.erl I did it as generic as I could so you can reuse it
easily, you have to pass an operation to it by calling flavio_op_fsm:op(N, W,
Op), where N and W are described above and where Op is a two item tuple, for
example for addition it would be {add, {A, B}}, it has to be that way so the
hashing is generic.

this fsm will then generate a RefId and will call our vnode with a command like
this: {RefId, Op} where Op is the two item tuple we passed to flavio_op_fsm:op.

flavio_op_fsm_sup is as generic as any fsm supervisor can get.

finally we register this new supervisor in our main supervisor tree in flavio_sup.erl::


    init(_Args) ->
        VMaster = { flavio_vnode_master,
                      {riak_core_vnode_master, start_link, [flavio_vnode]},
                      permanent, 5000, worker, [riak_core_vnode_master]},

        CoverageFSMs = {flavio_coverage_fsm_sup,
                        {flavio_coverage_fsm_sup, start_link, []},
                        permanent, infinity, supervisor, [flavio_coverage_fsm_sup]},

        OpFSMs = {flavio_op_fsm_sup,
                     {flavio_op_fsm_sup, start_link, []},
                     permanent, infinity, supervisor, [flavio_op_fsm_sup]},
        {ok,
            { {one_for_one, 5, 10},
              [VMaster, CoverageFSMs, OpFSMs]}}.

as before we add the OpFSMs definition and we add it to the list in the last
line.

we need to modify our vnode handle_command to handle the new command
format, that includes the RefId and has the parameters inside a tuple::

    handle_command({RefId, {add, {A, B}}}, _Sender, State=#state{ops_count=CurrentCount}) ->
        NewCount = CurrentCount + 1,
        NewState = State#state{ops_count=NewCount},
        {reply, {RefId, {A + B, State#state.partition}}, NewState};

and now instead of calculating the vnode ourselves we let out new flavio_op_fsm
take care of the call by changing the flavio:add implementation::

    add(A, B) ->
        N = 3,
        W = 3,
        Timeout = 5000,

        {ok, ReqID} = flavio_op_fsm:op(N, W, {add, {A, B}}),
        wait_for_reqid(ReqID, Timeout).

in this case we require 3 vnodes to run the command and we wait for the 3 to
consider the request successful, if the operation takes more than 5000 ms then
we fail with a timeout error.

the following line sends the desired N, W and the command in the new format, we
get back a request id we must wait for, we will receive a message to this
process with that ReqID and the result when all the requests finished or with
the error in case it failed or timed out::

        {ok, ReqID} = flavio_op_fsm:op(N, W, {add, {A, B}}),

to wait for the result we implement a function to do it for use::

    wait_for_reqid(ReqID, Timeout).

which is implemented as follows::

    wait_for_reqid(ReqID, Timeout) ->
        receive {ReqID, Val} -> {ok, Val}
        after Timeout -> {error, timeout}
        end.

let's rebuild and use it::

    $ rm -rf rel/flavio && make rel && ./rel/flavio/bin/flavio console

    (flavio@127.0.0.1)1> flavio:add(2, 4).
    {ok,[{6,433883298582611803841718934712646521460354973696},
         {6,388211372416021087647853783690262677096107081728},
         {6,411047335499316445744786359201454599278231027712}]}

    (flavio@127.0.0.1)2> flavio:add(12, 4).
    {ok,[{16,68507889249886074290797726533575766546371837952},
         {16,45671926166590716193865151022383844364247891968},
         {16,22835963083295358096932575511191922182123945984}]}

as you can see we get the 3 results back, it's our job to decide what to do
with them, we can pick one and return that one or we can compare all the
results to be sure that all vnodes got the same result, this is part of
conflict resolution and it should be part of the design decisions of your app.

the full change is here: https://github.com/marianoguerra/flaviodb/commit/dde9698c821055512b59fc54c25dbc5b223e8afe

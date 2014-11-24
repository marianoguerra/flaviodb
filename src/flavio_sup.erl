-module(flavio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

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

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, CoverageFSMs, OpFSMs]}}.

-module(mysql_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% Public API

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor behaviour callbacks

init([]) ->
  Pools = case application:get_env(mysql, pools) of
    {ok, P} ->
      P;
    _ ->
      []
  end,
  PoolSpecs = lists:map(fun({PoolName, SizeArgs, WorkerArgs}) ->
      PoolArgs = [{name, {local, PoolName}},
	      {worker_module, mysql_conn}]
	      ++ SizeArgs,
      poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
  end, Pools),
  Statements = ?CHILD(mysql_statement, worker, []),
  {ok, { {one_for_one, 5, 10}, [Statements | PoolSpecs]} }.

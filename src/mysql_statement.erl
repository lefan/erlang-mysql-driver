-module(mysql_statement).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
    start/0,
    stop/0,
    stop_and_cleanup/0,

    prepare/2,
    unprepare/1,
    get_prepared/1,
    get_prepared/2
  ]).

%% Internal exports - gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

%% Records
-include("mysql.hrl").

-record(state, {}).

%% Macros

-define(TABLE, mysql_statement_versions).

%% External functions

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [self()], []).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [self()], []).

stop() ->
  gen_server:call(?MODULE, stop).

%% This is here for use in tests.
%% Stops the process and deletes the ets table.
stop_and_cleanup() ->
  gen_server:call(?MODULE, stop),
  ets:delete(?TABLE).

%% @doc Register a prepared statement with the dispatcher. This call does not
%%   prepare the statement in any connections. The statement is prepared
%%   lazily in each connection when it is told to execute the statement.
%%   If the Name parameter matches the name of a statement that has
%%   already been registered, the version of the statement is incremented
%%   and all connections that have already prepared the statement will
%%   prepare it again with the newest version.
%%
%% @spec prepare(Name::atom(), Query::iolist()) -> ok
prepare(Name, Query) ->
  gen_server:cast(?MODULE, {prepare, Name, Query}).

%% @doc Unregister a statement that has previously been register with
%%   the dispatcher. All calls to execute() with the given statement
%%   will fail once the statement is unprepared. If the statement hasn't
%%   been prepared, nothing happens.
%%
%% @spec unprepare(Name::atom()) -> ok
unprepare(Name) ->
  gen_server:cast(?MODULE, {unprepare, Name}).

%% @doc Get the prepared statement with the given name.
%%
%%  This function is called from mysql_conn when the connection is
%%  told to execute a prepared statement it has not yet prepared, or
%%  when it is told to execute a statement inside a transaction and
%%  it's not sure that it has the latest version of the statement.
%%
%%  If the latest version of the prepared statement matches the Version
%%  parameter, the return value is {ok, latest}. This saves the cost
%%  of sending the query when the connection already has the latest version.
%%
%% @spec get_prepared(Name::atom(), Version::integer()) ->
%%   {ok, latest} | {ok, Statement::binary()} | {error, Err}
get_prepared(Name) ->
  get_prepared(Name, undefined).
get_prepared(Name, Version) ->
  gen_server:call(?MODULE, {get_prepared, Name, Version}).

%% gen_server callbacks

init([TableHeir]) ->
  EtsOptions = [set, named_table, public, {heir, TableHeir, undefined}],
  ets:new(?TABLE, EtsOptions),
  {ok, #state{}}.

handle_call({get_prepared, Name, Version}, _From, State) ->
  case ets:lookup(?TABLE, Name) of
    [] ->
      {reply, {error, {undefined, Name}}, State};
    [{Name, {_Statement, Version}}] ->
      {reply, {ok, latest}, State};
    [{Name, StatementInfo}] ->
      {reply, {ok, StatementInfo}, State}
  end;

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};

handle_call(_, _, State) ->
  {reply, error, State}.

handle_cast({prepare, Name, Statement}, State) ->
  NewVersion = case ets:lookup(?TABLE, Name) of
    [{Name, {_Existing, OldVersion}}] ->
      OldVersion + 1;
    [] ->
      1
  end,
  ets:insert(?TABLE, {Name, {Statement, NewVersion}}),
  {noreply, State};

handle_cast({unprepare, Name}, State) ->
  ets:delete(?TABLE, Name),
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_, State) ->
  {ok, State}.

-module(mysql_statement).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 start/0,
	 stop/0,

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

-record(state, {
	  %% maps names to {Statement::binary(), Version::integer()} values
	  prepares = gb_trees:empty()
	 }).

%% External functions

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

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

init([]) ->
  {ok, #state{}}.

handle_call({get_prepared, Name, Version}, _From, State) ->
    case gb_trees:lookup(Name, State#state.prepares) of
	none ->
	    {reply, {error, {undefined, Name}}, State};
	{value, {_StmtBin, Version1}} when Version1 == Version ->
	    {reply, {ok, latest}, State};
	{value, Stmt} ->
	    {reply, {ok, Stmt}, State}
    end;

handle_call(stop, _, State) ->
  {stop, normal, stopped, State};

handle_call(_, _, State) ->
  {reply, error, State}.

handle_cast({prepare, Name, Stmt}, State) ->
    Version1 =
	case gb_trees:lookup(Name, State#state.prepares) of
	    {value, {_Stmt, Version}} ->
		Version + 1;
	    none ->
		1
	end,
    {noreply, State#state{prepares =
			  gb_trees:enter(Name, {Stmt, Version1},
					  State#state.prepares)}};

handle_cast({unprepare, Name}, State) ->
    State1 =
	case gb_trees:lookup(Name, State#state.prepares) of
	    none ->
		State;
	    {value, _Stmt} ->
		State#state{prepares =
			    gb_trees:delete(Name, State#state.prepares)}
	end,
    {noreply, State1}.

handle_info(_, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, State) ->
    {ok, State}.

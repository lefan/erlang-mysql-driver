-module(mysql_statement_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

statement_test_() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun tests/1}.

start() ->
  {ok, Pid} = mysql_statement:start(),
  Pid.

stop(_) ->
  mysql_statement:stop_and_cleanup().

tests(_) ->
  mysql_statement:prepare(fuck, <<"this">>),
  First = mysql_statement:get_prepared(fuck),
  mysql_statement:prepare(fuck, <<"ittt">>),
  Second = mysql_statement:get_prepared(fuck),
  NonExistent = mysql_statement:get_prepared(doesntexist),
  [?_assertEqual({ok, {<<"this">>, 1}}, First),
   ?_assertEqual({ok, {<<"ittt">>, 2}}, Second),
   ?_assertEqual({error, {undefined, doesntexist}}, NonExistent)].

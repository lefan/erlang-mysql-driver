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
  Reply1 = mysql_statement:prepare(fuck, <<"this">>),
  First = mysql_statement:get_prepared(fuck),
  Reply2 = mysql_statement:prepare(fuck, <<"ittt">>),
  Reply3 = mysql_statement:prepare(fuck, <<"this">>),
  Second = mysql_statement:get_prepared(fuck),
  NonExistent = mysql_statement:get_prepared(doesntexist),
  [?_assertEqual(ok, Reply1),
   ?_assertEqual({error, statement_exists}, Reply2),
   ?_assertEqual(ok, Reply3),
   ?_assertEqual({ok, <<"this">>}, First),
   ?_assertEqual({ok, <<"this">>}, Second),
   ?_assertEqual({error, {undefined, doesntexist}}, NonExistent)].

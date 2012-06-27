-module(mysql_statement_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

statement_test() ->
  {ok, Pid} = mysql_statement:start(),
  mysql_statement:prepare(fuck, <<"this">>),
  ?assertEqual({ok, {<<"this">>, 1}}, mysql_statement:get_prepared(fuck)),
  mysql_statement:prepare(fuck, <<"ittt">>),
  ?assertEqual({ok, {<<"ittt">>, 2}}, mysql_statement:get_prepared(fuck)),
  ?assertEqual({error, {undefined, doesntexist}}, mysql_statement:get_prepared(doesntexist)).

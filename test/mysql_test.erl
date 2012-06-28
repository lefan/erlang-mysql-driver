-module(mysql_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("mysql.hrl").

-compile(export_all).

mysql_test_() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun basics/1}.

start() ->
  mysql_statement:start(),
  {ok, Pid} = mysql_conn:start("localhost", 3306, "root",
			       "", "erlang_mysql_driver_testdb",
			       fun(_,_,_,_) -> ok end, utf8, fuckitttt),
  Pid.
 
stop(Pid) ->
  mysql_statement:stop(),
  mysql_conn:stop(Pid).

basics(Pid) ->
  mysql_statement:prepare(insert_user, <<"INSERT INTO USERS (nickname) VALUES (?)">>),
  mysql_conn:execute(Pid, insert_user, 1, ["James"], self()),
  mysql_conn:execute(Pid, insert_user, ["James"], self()),
  {data, #mysql_result{rows=Rows}} = mysql_conn:fetch(Pid, <<"SELECT * FROM users">>, self()),
  [?_assertEqual([1, <<"James">>], lists:nth(1, Rows)),
   ?_assertEqual(2, length(Rows))].

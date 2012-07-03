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
  {ok, Pid} = mysql_conn:start(#mysql_connection_info{host="localhost",
						      port=3306,
						      user="root",
						      password="",
						      database="erlang_mysql_driver_testdb", 
						      log_fun=fun(_,_,_,_) -> ok end, 
						      encoding=utf8}, fuckitttt),
  Pid.
 
stop(Pid) ->
  mysql_statement:stop_and_cleanup(),
  mysql_conn:stop(Pid).

basics(Pid) ->
  mysql_statement:prepare(insert_user, <<"INSERT INTO USERS (nickname) VALUES (?)">>),
  mysql_conn:execute(Pid, insert_user, ["James"], self()),
  mysql_conn:execute(Pid, insert_user, ["James"], self()),
  {data, #mysql_result{rows=Rows}} = mysql_conn:fetch(Pid, <<"SELECT * FROM users">>),
  [?_assertEqual([1, <<"James">>], lists:nth(1, Rows)),
   ?_assertEqual(2, length(Rows))].

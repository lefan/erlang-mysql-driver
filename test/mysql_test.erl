-module(mysql_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("mysql.hrl").

-compile(export_all).

mysql_test() ->
  mysql_statement:start(),
  mysql_statement:prepare(insert_user, <<"INSERT INTO USERS (nickname) VALUES (?)">>),
  {ok, Pid} = mysql_conn:start("localhost", 3306, "root",
			       "", "erlang_mysql_driver_testdb",
			       fun(_,_,_,_) -> ok end, utf8, fuckitttt),
  mysql_conn:execute(Pid, insert_user, 1, ["James"], self()),
  {data, #mysql_result{rows=Rows}} = mysql_conn:fetch(Pid, <<"SELECT * FROM users">>, self()),
  ?assertEqual([[1, <<"James">>]], Rows).

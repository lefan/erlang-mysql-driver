all:
	./rebar compile

clean:
	./rebar clean

nothing:

createtestdb: nothing
	@mysql -uroot -e"drop database erlang_mysql_driver_testdb"
	@mysql -uroot -e"create database erlang_mysql_driver_testdb"
	@mysql -uroot erlang_mysql_driver_testdb < test/testdb.sql

test: createtestdb
	@./rebar eunit skip_deps=true

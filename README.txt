Description: Revamped MySQL driver for Erlang
Modified by: Yariv Sadan (yarivvv@gmail.com)
Modified again by: James Golick (jamesgolick@gmail.com)

This MySQL driver for Erlang is based on the Yxa driver obtained from Process One (at https://support.process-one.net/doc/display/CONTRIBS/Yxa). It includes several new features such as prepared statements, transactions, binary queries, type-converted query results, more efficient logging and a new connection pooling mechanism.

This fork (jamesgolick/erlang-mysql-driver) turns mysql_conn in to a gen_server and depends on poolboy for connection pooling.

It also makes preparing of statements immutable. There is no way to reuse names when preparing statements because that was a recipe for disaster anyway.

A lot of the logging stuff has been removed, but I'm working on that.

To configure, put something like this in sys.config:

 {
      mysql,
	[
	  {pools, [
	     {my_pool_name, [{size, 10},
			     {max_overflow, 20},
			     {host, "localhost"},
			     {port, 3306},
			     {database, "my_database"},
			     {user, "root"},
			     {password, ""},
			     {encoding, utf8}
			  ]},
	]
  } 

Then, start the otp application:

  application:start(mysql).

Then use the interface on the mysql module to query your database.

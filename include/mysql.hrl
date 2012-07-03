%% MySQL result record:
-record(mysql_result,
	{fieldinfo=[],
	 rows=[],
	 affectedrows=0,
	 insertid=0,
	 error="",
	 errcode=0,
	 errsqlstate=""}).

-record(mysql_connection_info, {
	host	    :: string(),
	port = 3306 :: integer(),
	user	    :: string(),
	password    :: string(),
	database    :: string(),
	log_fun     :: function(),
	encoding    :: atom()
    }).

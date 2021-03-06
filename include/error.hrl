%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-record(error, {code, message}).

-define(CATCH_ERROR(Expr, Args),
	try Expr
	catch
		throw:{error, Error} ->
			erlang:error(Error, Args)
	end).

-define(THROW_ERROR(E), throw(E)).
-define(THROW_GS_ERROR(E, State), throw({stop, normal, E, State})).

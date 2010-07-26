-module(conference). 
-export([start/1]).

start({[], _}) ->
	{reply, empty_conference_list};

start({Numbers, CallerEmail}) ->
	error_logger:info_msg("C: Start ~n"),
	case is_list(Numbers) of
				false ->
					error_logger:info_msg("C: no_member_list ~n"),
					{error, no_member_list};
				true ->
					lists:map(fun(Number) -> call(Number, CallerEmail) end, Numbers)
			
	end.

stop() ->
	exit(stopped).
	
%% PRIVATE FUNCTIONS %%

call(Number, CallerEmail) ->
	Normalised = normalise_number(Number),
	Pid = spawn( fun() -> outbound_call:call(Normalised, CallerEmail) end).

normalise_number(Number) ->
	NoDashes = re:replace(Number,"-","",[{return,list}, global]),
	NoSpaces = re:replace(NoDashes," ","",[{return,list}, global]),
	NoSpaces.


	

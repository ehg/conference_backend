-module(conference). 
-export([start/1]).

start([]) ->
	{reply, empty_conference_list};

start(Conference) ->
	error_logger:info_msg("C: Start ~n"),
	case is_list(Conference) of
				false ->
					error_logger:info_msg("C: no_member_list ~n"),
					{error, no_member_list};
				true ->
					lists:map(fun(Member) -> call(Member) end, Conference)
			
	end.

stop() ->
	exit(stopped).
	
%% PRIVATE FUNCTIONS %%

call(Number) ->
	Normalised = normalise_number(Number),
	Pid = spawn( fun() -> outbound_call:call(Normalised) end).

normalise_number(Number) ->
	NoDashes = re:replace(Number,"-","",[{return,list}, global]),
	NoSpaces = re:replace(NoDashes," ","",[{return,list}, global]),
	NoSpaces.


	

-module(conference). 
-export([start/1]).

start({[], _}) ->
	{reply, empty_conference_list};

start({Numbers, CallerEmail}) ->
	error_logger:info_msg("Conference: Start ~n"),
	case freeswitch:api(freeswitch@stan, create_uuid) of
		{ok, UUID} ->
				case is_list(Numbers) of
					false ->
							error_logger:info_msg("Conference: no_member_list ~n"),
							{error, no_member_list};
					true ->
							lists:map(fun(Number) -> call(Number, CallerEmail, UUID, length(Numbers)) end, Numbers)
				end;
		Else ->
			error_logger:error_msg("Conference: Create UUID failed: ~p ~n", [Else])
			
	end.

stop() ->
	exit(stopped).
	
%% PRIVATE FUNCTIONS %%

call(Number, CallerEmail, ConferenceUUID, ConferenceNumCalls) ->
	Normalised = normalise_number(Number),
	Pid = spawn( fun() -> outbound_call:call(Normalised, CallerEmail, ConferenceUUID, ConferenceNumCalls) end).

normalise_number(Number) ->
	NoDashes = re:replace(Number,"-","",[{return,list}, global]),
	NoSpaces = re:replace(NoDashes," ","",[{return,list}, global]),
	NoSpaces.


	

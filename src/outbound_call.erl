-module(outbound_call).
-export([call/5]).

call(OutgoingNumber, CallerEmail, ConferenceUUID, ConferenceNumCalls, Cli) ->
	%% we'll get our own unique UUID here, not sure why yet
	case freeswitch:api(freeswitch@localhost, create_uuid) of
		{ok, UUID} ->
			Args = "{origination_uuid="++UUID++ ",origination_caller_id_number=" ++ Cli ++
					",ignore_early_media=true,caller_email=" ++ CallerEmail ++
					",conf_num_calls="++ integer_to_list(ConferenceNumCalls) ++
					",billing_conference_uuid=" ++ ConferenceUUID ++
					"}sofia/gateway/gradwell/" ++ OutgoingNumber ++ " &park()",
			io:format("~p~n", [Args]),
			case freeswitch:bgapi(freeswitch@localhost, originate, Args) of
				{error, Reason} ->
					io:format("Error in origination command: ~p~n", [Reason]);
				{ok, _JobID} ->
					io:format("UUID: ~p~n", [UUID]),
					Gethandle = fun(Recusef, Count) ->
									io:format("Counted ~p", [Count]),
									case freeswitch:handlecall(freeswitch@localhost, UUID) of
										{error, badsession} when Count > 4 ->
												{error, badsession};
										{error, badsession} ->
												timer:sleep(100),
												Recusef(Recusef, Count+1);
										{error, Other} ->
												{error, Other};
										Else ->
												Else
									end
					end,
					case Gethandle(Gethandle, 0) of
							{error, badsession} ->
								io:format("bad uuid ~p", [UUID]),
								{stop, {error, session}};
							{error, Other} ->
								io:format("other error starting; ~p", [Other]),
								{stop, {error, Other}};
							_Else ->
								io:format("starting for ~pn", [UUID]),
								put(uuid, UUID),
								put(conference_uuid, ConferenceUUID),
								wait_for_park()
					end				
			end;
			
		Else ->
			io:format("Couldn't generate UUID: ~p~n", [Else])
	end.

wait_for_park() ->
	receive 
		{channel_destroy, UUID} ->
			%%error_logger:info_msg("myhandler ~p: destroyed channel ~p~n",[self(), UUID]),
			wait_for_park();
		{call, Data} ->
			%%error_logger:info_msg("myhandler ~p: call ~p~n",[self(), freeswitch:get_event_name(Data)]),
			wait_for_park();
		{call_event, Data} ->
			EventName = freeswitch:get_event_name(Data),
			{event, [UUID | Rest]} = Data,
			%%error_logger:info_msg("myhandler ~p: call event ~p ~p ~n",[self(), EventName, UUID ]),
			
			
			case EventName of 
				"CHANNEL_ANSWER" ->
					error_logger:info_msg("myhandler ~p: call answered~p~n"),
					set_call_start_time(proplists:get_value("Event-Date-Timestamp", Rest));					
				"CHANNEL_PARK" ->
					error_logger:info_msg("myhandler ~p: call parked~p~n"),
					press_one_to_listen();	
				"CHANNEL_HANGUP" ->
					process_hangup(proplists:get_value("Hangup-Cause", Rest));
				"CHANNEL_HANGUP" ->
					%% exit here? cause values?
					disconnect();
				_
					->
					not_recognised
			end,
			wait_for_park();
		{command, disconnect} ->
			disconnect();
		{What , Data} ->
			%%error_logger:info_msg("myhandler ~p: unknown event (~p) ~p~n",[self(), What, freeswitch:get_event_name(Data)]),
			wait_for_park()
	end.

process_hangup(Cause) ->
	error_logger:info_msg("Hangup detected, cause ~p~n",[Cause]),
	disconnect().
	
set_call_start_time(Timestamp) ->
	error_logger:info_msg("myhandler ~p: Time stamp ~pn",[self(), Timestamp]).
	
play(UUID, File) ->
	freeswitch:sendmsg(freeswitch@localhost, UUID, [{"call-command", "execute"}, 
															{"event-lock", "true"}, 
															{"execute-app-name", "playback"}, 
															{"execute-app-arg", "/usr/local/freeswitch/sounds/en/us/callie/conference/8000/" ++ File}]),
	wait_for_execute_complete().
	

wait_for_execute_complete() ->
	receive 
		{call_event, Data} ->
			EventName = freeswitch:get_event_name(Data),
			{event, [UUID | Rest]} = Data,

			case EventName of 
				"CHANNEL_EXECUTE_COMPLETE" ->
					timer:sleep(100),
					error_logger:info_msg("~p : execution complete ~n",[self()]),
					execute_complete;
				"DTMF" ->
					Dtmf = proplists:get_value("DTMF-Digit", Rest),
					
					case freeswitch:bgapi(freeswitch@localhost, break, get(uuid) ++ " all") of
						{error, Reason} ->
							io:format("Error in break command: ~p~n", [Reason]);
						{ok, _JobID} ->
							error_logger:info_msg("~p : ~p barged by pressing ~p ~n",[self(), UUID, Dtmf]),
							timer:sleep(100),
							self() ! {call_event, Data}
					end;
				"CHANNEL_HANGUP" ->
					%% exit here? cause values?
					disconnect();
				Other ->
					error_logger:info_msg("~p : unexpected event: ~p~n", [self(), Other]),
					wait_for_execute_complete()
			end;
		{command, disconnect} ->
			disconnect()
	end.	
	
bridge_to_conference(UUID) ->
	error_logger:info_msg("~p : Putting ~p in the conference: ~n",[self(),UUID]),
	freeswitch:sendmsg(freeswitch@localhost, UUID, [{"call-command", "execute"}, 
															{"event-lock", "true"}, 
															{"execute-app-name", "conference"}, 
															{"execute-app-arg", get(conference_uuid)}]),
														%% need error checking
	wait_for_execute_complete(),
	in_conference().
	

in_conference() ->
	receive 
		{call_event, Data} ->
			EventName = freeswitch:get_event_name(Data),
			{event, [UUID | Rest]} = Data,

			case EventName of 			
				"CHANNEL_HANGUP" ->
					%% exit here? cause values?
					disconnect();
					
				"CHANNEL_HANGUP_COMPLETE" ->
					%% exit here?
					disconnect();
						
				_ ->
					in_conference()
			end;
		{command, disconnect} ->
			freeswitch:bgapi(freeswitch@localhost, hangup, "NORMAL_CLEARING"),
			disconnect()
	end.

press_one_to_listen() ->
	press_one_to_listen(0).
press_one_to_listen(5) ->
	disconnect();
press_one_to_listen(Count) ->
	play(get(uuid), "invited.wav"),
	receive 
		{call_event, Data} ->
			EventName = freeswitch:get_event_name(Data),
			{event, [UUID | Rest]} = Data,

			case EventName of 
				"DTMF" ->
					Dtmf = proplists:get_value("DTMF-Digit", Rest),
					error_logger:info_msg("myhandler : ~p pressed ~p ~n",[UUID, Dtmf]),
					case Dtmf of
						"1" ->
							play(get(uuid), "takenote.wav"),
							bridge_to_conference(get(uuid));
						_ 	->
							disconnect()
					end;
				"CHANNEL_HANGUP" ->
					%% exit here? cause values?
					disconnect();
					
				"CHANNEL_HANGUP_COMPLETE" ->
					%% exit here?
					disconnect();
											
				_ ->
					press_one_to_listen(Count)
			end;
						
		{Event, Data} ->
			EventName = freeswitch:get_event_name(Data),
			io:format("E: ~p N: ~p~n", [Event, EventName]),
			press_one_to_listen(Count)
	after 5000 ->
		press_one_to_listen(Count + 1)
	end.
	
														
disconnect() ->
	error_logger:info_msg("myhandler : Disconnecting~n"),
	%% need to send disconnected event?
	exit(disconnect).
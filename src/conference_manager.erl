-module(conference_manager).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3, start_conference/1]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

start_conference(Conference) ->
	gen_server:call(?MODULE, {start_conference, Conference}).

%% CALLBACKS


	
init([]) ->
	{ok, ets:new(?MODULE, [])}.

handle_call({start_conference, Conference}, _From, State) ->
	error_logger:info_msg("CM: Handle call ~n"),
	Pid = spawn( fun() -> conference:start(Conference) end),
	{reply, Pid, State};
	
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.
	
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% INTERNAL
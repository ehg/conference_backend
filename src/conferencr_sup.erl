%%% -------------------------------------------------------------------
%%% Author  : chris
%%% Description :
%%%
%%% Created : 27 Feb 2010
%%% -------------------------------------------------------------------
-module(conferencr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ConferenceManagerSup = {conference_manager_sup,
                    {conference_manager_sup, start_link,[]},
                    permanent, infinity, supervisor, [conference_manager_sup]},
	ApiWebserverSup = {api_webserver_sup,
                    {api_webserver_sup, start_link,[5050]},
                    permanent, infinity, supervisor, [api_webserver_sup]},
	
    {ok,{{one_for_one,5,60}, [ConferenceManagerSup, ApiWebserverSup]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_port() ->
    {ok, Result} = application:get_env(conferencr, service_port),
    Result.
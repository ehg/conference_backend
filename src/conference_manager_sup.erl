%%% -------------------------------------------------------------------
%%% Author  : chris
%%% Description :
%%%
%%% Created : 28 Feb 2010
%%% -------------------------------------------------------------------
-module(conference_manager_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
process_flag(trap_exit, true),
  error_logger:info_msg("~p (~p) starting...~n", [?MODULE, self()]),
  ConferenceManager = {conference_manager, {conference_manager, start_link, []},
		   permanent, 5000, worker, [conference_manager]},
  {ok, {{one_for_all, 5, 30}, [ConferenceManager]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


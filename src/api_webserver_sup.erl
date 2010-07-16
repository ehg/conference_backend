%% Author: chris
%% Created: 28 Feb 2010
%% Description: TODO: Add description to api_webserver_sup
-module(api_webserver_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
process_flag(trap_exit, true),
  error_logger:info_msg("~p (~p) starting...~n", [?MODULE, self()]),
  ApiWebserver = {api_webserver, {api_webserver, start_link, [Port]},
		   permanent, 5000, worker, [api_webserver]},
  {ok, {{one_for_all, 5, 30}, [ApiWebserver]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

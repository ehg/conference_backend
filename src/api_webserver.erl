%% Author: chris
%% Created: 27 Feb 2010
%% Description: TODO: Add description to api_webserver
-module(api_webserver).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(OK, <<"ok">>).

%% API
-export([start_link/1, dispatch_requests/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([Port]) ->
  mochiweb_http:start([{port, Port},
		       {loop, fun(Req) -> dispatch_requests(Req) end}]),
  erlang:monitor(process, mochiweb_http),
  {ok, []}.

stop() ->
  gen_server:cast(?SERVER, stop).

dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _, _, {mochiweb_http, _}, _}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  mochiweb_http:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle("/Conference.svc/Start", Req) ->
 	Data = de_null(Req:recv_body()),
	error_logger:info_msg("DATA ~p~n", [Data]),
	
	Struct = mochijson2:decode(Data),
 	error_logger:info_msg("STRUCT ~p~n", [Struct]),
 	%% { "email" : "blah@blah.com", "password" : "HASH", 
	%%   "numbers" : ["07800813656", "08451232212"] }
	CallerEmail = binary_to_list(struct:get_value(<<"email">>, Struct)),
	Authorization = Req:get_header_value("Authorization"),
 	Numbers = struct:get_value(<<"numbers">>, Struct),

	% check authorisation with conference manager
 	{AuthResult, Cli} = conference_manager:authorise(Authorization, Numbers),
		
	case AuthResult of
		200 -> Conference = {Numbers, CallerEmail, Cli},
				conference_manager:start_conference(Conference),
				success(Req, "success");
		_ ->	error(Req, {AuthResult, Cli}) 
	end;

handle(_, Req) ->
  Req:not_found().

error(Req, {Code, Body}) ->
  Req:respond({Code, [{"Content-Type", "application/json"}], Body}).

unauthorised(Req) ->
  Req:respond({401, [{"Content-Type", "application/json"}], "Unauthorised."}).

notfound(Req, Body) ->
  Req:respond({404, [{"Content-Type", "application/json"}], Body}).

success(Req, Body) ->
  Req:respond({200, [{"Content-Type", "application/json"}], Body}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.

de_null(B) -> de_null(B, 0).

de_null(Binary, Offset) ->
  case Binary of
    <<Stuff:Offset/binary, 0, _/binary>> -> binary_to_list(Stuff);
    <<Stuff:Offset/binary, _:0/binary>> -> binary_to_list(Stuff);
    _ -> de_null(Binary, Offset + 1)
  end.


%% Author: chris
%% Created: 24 Jul 2010
%% Description: TODO: Add description to authorise
-module(authorise).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([authorise/1]).

%%
%% API Functions
%%
authorise(Token) ->
	% send authorisation request
	request_authorisation(Token).

%%
%% Local Functions
%%

request_authorisation(Token) ->
	% make http request
	URL = "http://switch.ucf.org.uk:3000",
	case http:request(get, {URL, [{"Authorization", Token}]}, [], []) of
		{ok, {{_Version, Code, _ReasonPhrase}, _Headers, Body}} ->
			parse_response(Code, Body);
		_ -> 
			false
	end.


parse_response(Code, Body) ->
	case Code of
		200 -> {true, Body};
		_ -> {false, authorisation_error}
	end.

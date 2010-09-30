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
-export([authorise/2]).

%%
%% API Functions
%%
authorise(Token, Numbers) ->
	% send authorisation request
	request_authorisation(Token, Numbers).

%%
%% Local Functions
%%

request_authorisation(Token, Numbers) ->
	Json = to_json(Numbers),
	io:format("JSON auth: ~p~n", [Json]),
	% make http request
	ssl:start(),
	URL = "https://conferencer.heroku.com/authorise",
	case http:request(post, {URL, [{"Authorization", Token}], "application/json", Json}, [], []) of
		{ok, {{_Version, Code, _ReasonPhrase}, _Headers, Body}} ->
			{Code, Body};
		_ -> 
			{408, request_timeout}
	end.

to_json(Numbers) ->
	to_json(Numbers, "{ \"numbers\" : [").

to_json([], Output) ->
	Output ++  "]}";
to_json([H|Numbers], Output) ->
	case Numbers of
        [] ->
            to_json(Numbers, Output ++ "\"" ++ binary_to_list(H) ++ "\"");
        _ ->
        	to_json(Numbers, Output ++ "\"" ++ binary_to_list(H) ++ "\",")
	end.

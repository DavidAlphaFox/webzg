%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% Handler middleware.
%%
%% Execute the handler given by the <em>handler</em> and <em>handler_opts</em>
%% environment values. The result of this execution is added to the
%% environment under the <em>result</em> value.

%% The cowboy_handler middleware executes the handler passed through the environment values handler and handler_opts,
%% and adds the result of this execution to the environment as the value result,
%% indicating that the request has been handled and received a response.

%% This module also defines the cowboy_handler behaviour that defines the basic interface for handlers.
%% All Cowboy handlers implement at least the init/2 callback,
%% and may implement the terminate/3 callback optionally.
-module(cowboy_handler).
-behaviour(cowboy_middleware).

-export([execute/2]).
-export([terminate/4]).


%% Callback init/3
%% Process the request.
%%
%% This function can be used to switch to an alternate handler type
%% by returning the name of the module to be used, along with a few options.
%%
%% For basic handlers this is the function where the response should be sent.
%% If no response is sent, Cowboy will ensure that a 204 No Content response is sent.
%%
%% A crash in this callback will result in terminate/3 being called if it is defined,
%% with the State argument set to the value of Opts originally given to the init/2 callback.
-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), hibernate}
	| {module(), Req, any(), timeout()}
	| {module(), Req, any(), timeout(), hibernate}
	when Req::cowboy_req:req().

%% Callback terminate/3
%% The following values may be received as the terminate reason in the optional terminate/3 callback.
%% Different handler types may define additional terminate reasons.
%%
%% normal
%%
%%      The connection was closed normally.
%%
%% {crash, Class, Reason}
%%
%%     A crash occurred in the handler.
%%     Class and Reason can be used to obtain more information about the crash.
%%     The function erlang:get_stacktrace/0 can also be called to
%%     obtain the stacktrace of the process when the crash occurred.
%% @todo optional -callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.

-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
	{_, Handler} = lists:keyfind(handler, 1, Env),
	{_, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
	try Handler:init(Req, HandlerOpts) of
		{ok, Req2, State} ->
			Result = terminate(normal, Req2, State, Handler),
			{ok, Req2, [{result, Result}|Env]};
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, run);
		{Mod, Req2, State, hibernate} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, hibernate);
		{Mod, Req2, State, Timeout} ->
			Mod:upgrade(Req2, Env, Handler, State, Timeout, run);
		{Mod, Req2, State, Timeout, hibernate} ->
			Mod:upgrade(Req2, Env, Handler, State, Timeout, hibernate)
	catch Class:Reason ->
		Stacktrace = erlang:get_stacktrace(),
		cowboy_req:maybe_reply(Stacktrace, Req),
		terminate({crash, Class, Reason}, Req, HandlerOpts, Handler),
		exit({cowboy_handler, [
			{class, Class},
			{reason, Reason},
			{mfa, {Handler, init, 2}},
			{stacktrace, Stacktrace},
			{req, cowboy_req:to_list(Req)},
			{opts, HandlerOpts}
		]})
	end.



-spec terminate(any(), Req, any(), module()) -> ok when Req::cowboy_req:req().
terminate(Reason, Req, State, Handler) ->
	case erlang:function_exported(Handler, terminate, 3) of
		true ->
			try
				Handler:terminate(Reason, cowboy_req:lock(Req), State)
			catch Class:Reason2 ->
				exit({cowboy_handler, [
					{class, Class},
					{reason, Reason2},
					{mfa, {Handler, terminate, 3}},
					{stacktrace, erlang:get_stacktrace()},
					{req, cowboy_req:to_list(Req)},
					{state, State},
					{terminate_reason, Reason}
				]})
			end;
		false ->
			ok
	end.

%% Feel free to use, reuse and abuse the code in this file.

%% @doc Pastebin handler.
-module(api_handler).

%% Standard callbacks.
-export([init/2]).
-include("webzg.hrl").
-include("logger.hrl").
-include("api.hrl").
-include("code.hrl").
-record(state, {a,b,c}).
-compile(export_all).

init(Req, Opts) ->
  random:seed(now()),
  {cowboy_rest, Req, #state{}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.


content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_post}],
    Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, handle_req}
  ], Req, State}.

accept_error(Req, State) ->
  {true, Req, State}.



handle_req(Req, State) ->
  Response = handle(get, Req, State),
  {jsx:encode(Response), Req, State}.

handle_post(Req, State) ->
  Response = handle(post, Req, State),
  Req2 = cowboy_req:set_resp_body(jsx:encode(Response), Req),
  {true, Req2, State}.


handle(Method, Req, State) ->
 case catch pt:parse(Method, Req, State) of
   {'EXIT', Reason} ->
     ?DEBUG("handle http error:~p", [Reason]),
     #{<<"code">> => ?UNKNOWN_CMD};
   Res ->
     Res
 end.
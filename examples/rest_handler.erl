%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(rest_handler).

-export([init/2]).
%% optional callbacks
-export([
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2
  ]).

-export([handle/2]).

init(Req, Opts) ->
  random:seed(now()),
  {cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.



content_types_provided(Req, State) ->
  {[
    {<<"text/html">>, handle},
    {<<"application/json">>, handle}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {'*', handle_post},
    {{<<"text">>, <<"plain">>, [{<<"charset">>, <<"utf-8">>}]},handle_post},
    {{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_post}],
    Req, State}.

handle(Req, State) ->
  Method = cowboy_req:method(Req),
  Body =
    case Method of
    <<"GET">> -> json();
      _ -> handle_post(Req, State)
    end,
  {Body, Req, State}.


json() ->
  "{\"key\":\"val\", \"status\":\"500\"}".

handle_post(Req, State) ->
  PidInfo = pid_info(),
  Req2= cowboy_req:set_resp_body(PidInfo, Req),
  Body = "{\"key\":\"val\", \"status\":\"200\"}",
%%  cowboy_req:reply(200, [], Body, Req),
  {true, Req2, State}.

pid_info() ->
  Processes = erlang:processes(),
  Info = erlang:process_info(hd(Processes)),
  lists:flatten(io_lib:format("~p", [Info])).
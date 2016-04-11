%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 三月 2016 16:39
%%%-------------------------------------------------------------------
-module(webzg).
-author("zhaogang").

-export([start/0, stop/0, start_app/1]).



start() ->
  start_apps(),
  webzg_logger:start(),
  webzg_tpl:init(),
  application:start(webzg).

stop() ->
  %% application:stop(webzg)
  application:stop(webzg).


start_apps() ->
  start_app(crypto),
  start_app(sasl),
  start_app(asn1),
  start_app(public_key),
  start_app(ssl),
  start_app(inets), %% for httpc service
  start_app(ranch),
  start_app(cowlib),
  start_app(cowboy), %% for httpc service
  ok.


start_app(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok;
    Err ->
      throw({App, Err})
  end.


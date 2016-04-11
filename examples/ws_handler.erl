%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 二月 2016 10:30
%%%-------------------------------------------------------------------
-module(ws_handler).
-author("zhaogang").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
  erlang:start_timer(5000, self(), <<"Hello!">>),
  {cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
  {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(10000, self(), <<"How' you doin'?">>),
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 四月 2016 17:05
%%%-------------------------------------------------------------------
-module(pt_people).
-author("zhaogang").

%% API
-export([parse_get/2, parse_post/2, write/2]).
-include("webzg.hrl").
-include("logger.hrl").
-include("code.hrl").

parse_get([<<"history">>], Req) ->
  case account:check_auth(Req) of
    {true, User} ->
      QueryList = cowboy_req:parse_qs(Req),
%%      {_, Number} = lists:keyfind(<<"number">>, 1, QueryList),
%%      {_, Date} = lists:keyfind(<<"date">>, 1, QueryList),
      {ok, history, [User, 1, 1]};
    {false, ErrCode} ->
      {error, ErrCode}
  end.

parse_post([<<"history">>], Req) ->
  case account:check_auth(Req) of
    {true, User} ->
      {ok, QueryList, _Req2} = cowboy_req:body_qs(Req),
      {_, Number} = lists:keyfind(<<"number">>, 1, QueryList),
      {_, Date} = lists:keyfind(<<"date">>, 1, QueryList),
      {ok, history, [User, Number, Date]};
    {false, ErrCode} ->
      {error, ErrCode}
  end.

write([<<"history">>], Posts) ->
  #{<<"code">> => ?CODE_SUCCESS,
    <<"data">> => Posts
    }.
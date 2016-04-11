%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% normalize协议
%%% @end
%%% Created : 14. 三月 2016 11:34
%%%-------------------------------------------------------------------
-module(pt_normalize).
-author("zhaogang").

%% API
-export([parse_post/2, write/2]).
-include("webzg.hrl").
-include("logger.hrl").
-include("code.hrl").

parse_post([], Req) ->
  case account:check_auth(Req) of
    {true, User} ->
      {ok, QueryList, _Req2} = cowboy_req:body_qs(Req),
      {_, Number} = lists:keyfind(<<"number">>, 1, QueryList),
      {_, Date} = lists:keyfind(<<"date">>, 1, QueryList),
      {ok, normalize_query, [User, Number, Date]};
    {false, ErrCode} ->
      {error, ErrCode}
  end.

write([], NormalizeData) ->
  #{<<"code">> => ?CODE_SUCCESS,
    <<"data">> => #{
      <<"normalize">> => NormalizeData
    }}.
%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 三月 2016 11:13
%%%-------------------------------------------------------------------
-module(pt_account).
-author("zhaogang").
-include("code.hrl").
%% API
-export([parse_get/2, parse_post/2, write/2]).


parse_post([<<"login">>], Req) ->
  QueryList = cowboy_req:body_qs(Req),
  {_, User} = lists:keyfind(<<"username">>, 1, QueryList),
  {_, Password} = lists:keyfind(<<"password">>, 1, QueryList),
  {IP, _Port} = cowboy_req:peer(Req),
  {ok, login, [User, Password, IP]}.

parse_get([<<"login">>], Req) ->
  QueryList = cowboy_req:parse_qs(Req),
  {_, User} = lists:keyfind(<<"username">>, 1, QueryList),
  {_, Password} = lists:keyfind(<<"password">>, 1, QueryList),
  {IP, _Port} = cowboy_req:peer(Req),
  {ok, login, [User, Password, IP]};

parse_get([<<"session">>], _Req) ->
  {ok, session, []}.


write([<<"login">>], {Token, Expires}) ->
  #{<<"code">> => ?CODE_SUCCESS,
    <<"data">> => #{<<"token">> => Token,
                   <<"expires">> => list_to_binary(integer_to_list(Expires))}};
write([<<"session">>], Data) ->
  #{<<"code">> => ?CODE_SUCCESS,
    <<"data">> => #{<<"session">> => list_to_binary(Data)}}.

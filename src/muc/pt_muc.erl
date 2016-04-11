%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 三月 2016 10:23
%%%-------------------------------------------------------------------
-module(pt_muc).
-author("zhaogang").
-include("code.hrl").
-include("logger.hrl").
%% API
-export([parse_get/2, parse_post/2, write/2]).



parse_get([<<"trooplist">>], _) ->
  {ok, get_rooms, []}.

parse_post(_, _) ->
  ok.

write([<<"trooplist">>], MucData) ->
  #{<<"code">> => ?CODE_SUCCESS,
    <<"data">> => #{
      <<"normalize">> => func:term_to_string(MucData)
    }}.
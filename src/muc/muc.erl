%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 三月 2016 11:30
%%%-------------------------------------------------------------------
-module(muc).
-author("zhaogang").

%% API
-export([get_rooms/1]).

get_rooms([]) ->
  ets:tab2list(muc_online_room).
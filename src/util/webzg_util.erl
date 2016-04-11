%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 二月 2016 12:11
%%%-------------------------------------------------------------------
-module(webzg_util).
-author("zhaogang").

%% API
-export([new_id/0, new_id/2]).

%% 获取一个随机ID
new_id() ->
  Initial = random:uniform(62) - 1,
  new_id(<<Initial>>, 7).

new_id(Bin, 0) ->
  Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
  << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
new_id(Bin, Rem) ->
  Next = random:uniform(62) - 1,
  new_id(<<Bin/binary, Next>>, Rem - 1).



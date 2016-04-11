%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 四月 2016 17:06
%%%-------------------------------------------------------------------
-module(people).
-author("zhaogang").
-include("upload.hrl").
%% API
-export([history/1]).


history([User, Number, Date]) ->
  Records = mnesia:dirty_index_read(file_record, User, #file_record.user),
  lists:map(fun(#file_record{file = File, timestamp = Time, desc = Desc}) ->
    #{<<"url">> => File,
      <<"timestamp">> => Time,
      <<"desc">> => Desc
      }
    end, Records).
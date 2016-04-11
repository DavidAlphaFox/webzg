%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 时间函数
%%% @end
%%% Created : 01. 四月 2016 12:20
%%%-------------------------------------------------------------------
-module(lib_time).
-author("zhaogang").

%% API
-export([timestamp/0,
  iso_time/0,
  iso_time/1,
  get_time/0,
  get_date/0]).


%% 获取标准时间戳
timestamp() ->
  BaseSeconds  = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  NowSeconds   = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  NowSeconds - BaseSeconds.


%% 输出标准时间的字符串 "20151223 11:23:06"
iso_time() ->
  iso_time(erlang:now()).

iso_time(ErlStamp) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(ErlStamp),
  iolist_to_binary(io_lib:format("~4..0B~2..0B~2..0B ~2..0B:~2..0B:~2..0B",
    [Year, Month, Day, Hour, Minute, Second])).


-spec get_time() -> list().
get_time() ->
  {{Y,M,D},{H,Min,S}} =calendar:local_time(),
  Trans = fun(Num) ->
    if
      Num >9  -> integer_to_list(Num) ;
      true -> "0" ++ integer_to_list(Num)
    end
   end,
  Trans(Y) ++ "-" ++ Trans(M) ++ "-" ++ Trans(D)
    ++ " " ++ Trans(H) ++ ":" ++ Trans(Min) ++":" ++ Trans(S).


-spec get_date() -> list().
get_date() ->
  NowStamp = timestamp(),
  {{Y, M, D}, _Time} = calendar:gregorian_seconds_to_datetime(NowStamp),
  Trans = fun(Num) ->
    if
      Num >9 -> integer_to_list(Num);
      true -> "0" ++ integer_to_list(Num)
    end
  end,
  Trans(Y) ++ Trans(M) ++ Trans(D).
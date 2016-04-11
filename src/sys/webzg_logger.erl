%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 二月 2016 15:01
%%%-------------------------------------------------------------------
-module(webzg_logger).
-author("zhaogang").
-include("webzg.hrl").
-include("logger.hrl").
%% API
-export([start/0, test/1]).

start() ->
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  application:load(lager),
  ConsoleLog = get_log_path(),
  Dir = filename:dirname(ConsoleLog),
  ErrorLog = filename:join([Dir, "error.log"]),
  CrashLog = filename:join([Dir, "crash.log"]),
  LogRotateDate = get_pos_string_env(log_rotate_date, ""),
  LogRotateSize = get_pos_integer_env(log_rotate_size, 10*1024*1024),
  LogRotateCount = get_pos_integer_env(log_rotate_count, 1),
  LogRateLimit = get_pos_integer_env(log_rate_limit, 100),
  application:set_env(lager, error_logger_hwm, LogRateLimit),
  application:set_env(
  lager, handlers,
  [{lager_console_backend, info},
  {lager_file_backend, [{file, ConsoleLog}, {level, debug}, {date, LogRotateDate},
  {count, LogRotateCount}, {size, LogRotateSize}]},
  {lager_file_backend, [{file, ErrorLog}, {level, error}, {date, LogRotateDate},
  {count, LogRotateCount}, {size, LogRotateSize}]}]),
  application:set_env(lager, crash_log, CrashLog),
  application:set_env(lager, crash_log_date, LogRotateDate),
  application:set_env(lager, crash_log_size, LogRotateSize),
  application:set_env(lager, crash_log_count, LogRotateCount),
  lager:start().


get_log_path() ->
  case application:get_env(ejabberd, log_path) of
    {ok, Path} ->
      Path;
    undefined ->
      case os:getenv("EJABBERD_LOG_PATH") of
        false ->
          ?LOG_PATH;
        Path ->
          Path
      end
  end.


get_pos_integer_env(Name, Default) ->
  case application:get_env(ejabberd, Name) of
    {ok, I} when is_integer(I), I>0 ->
      I;
    undefined ->
      Default;
    {ok, Junk} ->
      error_logger:error_msg("wrong value for ~s: ~p; "
      "using ~p as a fallback~n",
        [Name, Junk, Default]),
      Default
  end.
get_pos_string_env(Name, Default) ->
  case application:get_env(ejabberd, Name) of
    {ok, L} when is_list(L) ->
      L;
    undefined ->
      Default;
    {ok, Junk} ->
      error_logger:error_msg("wrong value for ~s: ~p; "
      "using ~p as a fallback~n",
        [Name, Junk, Default]),
      Default
  end.

test(Msg) ->
  ?DEBUG("~p~n", [Msg]),
  ?INFO_MSG("~p~n", [Msg]),
  ?WARNING_MSG("~p~n", [Msg]),
  ?ERROR_MSG("~p~n", [Msg]).
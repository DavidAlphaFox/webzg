%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 三月 2016 9:54
%%%-------------------------------------------------------------------
-module(pt).
-author("zhaogang").
-include("logger.hrl").
%% API
-export([parse/3]).

%% 根据协议将解析请求
parse(get, Req, State) ->
  [BinMod|SubReq] = cowboy_req:path_info(Req),
  ParseMod = binary_to_atom(<<"pt_", BinMod/binary>>, utf8),
  case ParseMod:parse_get(SubReq, Req) of
    {ok, Func, Args} ->
      Mod = binary_to_atom(<<BinMod/binary>>, utf8),
      ?DEBUG("~p~n", [Mod:Func(Args)]),
      case Mod:Func(Args) of
        {error, Code} ->
          #{<<"code">> => Code};
        Result ->
          ParseMod:write(SubReq, Result)
      end;
    {error, Code} ->
      #{<<"code">> => Code}
  end;

parse(post, Req, State) ->
  [BinMod|SubReq] = cowboy_req:path_info(Req),
  ParseMod = binary_to_atom(<<"pt_", BinMod/binary>>, utf8),
  case ParseMod:parse_post(SubReq, Req) of
    {ok, Func, Args} ->
      Mod = binary_to_atom(<<BinMod/binary>>, utf8),
      case Mod:Func(Args) of
        {error, Code} ->
          #{<<"code">> => Code};
        Result ->
          ParseMod:write(SubReq, Result)
      end;
    {error, Code} ->
      #{<<"code">> => Code}
  end.
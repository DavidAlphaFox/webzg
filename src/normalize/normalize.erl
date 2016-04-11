%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 三月 2016 11:39
%%%-------------------------------------------------------------------
-module(normalize).
-author("zhaogang").
-include("webzg.hrl").
-include("logger.hrl").
-include("code.hrl").
%% API
-export([normalize_query/1]).


normalize_query([_User, Number, Date]) ->
  NormalizeData = <<Number/binary, "date=", Date/binary>>,
  NormalizeData.

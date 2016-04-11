%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 三月 2016 17:03
%%%-------------------------------------------------------------------
-module(webzg_db).
-author("zhaogang").

%% API
-export([init/1]).

%% 初始化数据库
init(Opts) ->
  DbNodes = mnesia:system_info(db_nodes),
  true =  lists:member(node(), DbNodes),
  case mnesia:system_info(extra_db_nodes) of
    [] ->
      mnesia:create_schema([node()]);
    _ ->
      ok
  end,
  mnesia:start(),
  mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
  db_account:init(Opts),
  upload:init(Opts).
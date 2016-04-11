%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 二月 2016 16:17
%%%-------------------------------------------------------------------
-module(webzg_tpl).
-author("zhaogang").

%% API
-export([init/0]).

init() ->
  TplDir = webzg_path:path(template),
  {ok, _} = erlydtl:compile(TplDir ++ "/cookie.html", tpl_cookie),
  {ok, _} = erlydtl:compile(TplDir ++ "/page404.html", tpl_page404).
%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%更新升级相关
%%% @end
%%% Created : 25. 三月 2016 16:34
%%%-------------------------------------------------------------------
-module(webzg_updater).
-author("zhaogang").

%% API
-export([update/0, soft_update/0]).



%% 更新代码
update() ->
  UnPurgedMods = mods_to_update(),
  lists:foreach(fun(Mod) ->
    true = code:purge(Mod)
                end, UnPurgedMods),
  [code:load_file(Mod)|| Mod <- UnPurgedMods].

%% 不kill进程更新
soft_update() ->
  UnPurgedMods = mods_to_update(),
  ok = soft_purge(UnPurgedMods),
  [code:load_file(Mod)|| Mod <- UnPurgedMods].

%% 获取要更新的模块
mods_to_update() ->
  Path = webzg_path:code_dir(),
  {ok, Files} = file:list_dir(Path),
  lists:filtermap(fun(File) ->
    case lists:suffix(".beam", File) of
      false -> false;
      true ->
        {true, list_to_atom(filename:rootname(filename:basename(File)))}
    end
                  end, Files).

%% soft purge, 如果该mod被占用,则等待1秒后再执行purge
soft_purge(UnPurgedMods) ->
  Fun = fun(Mod) ->
    case code:soft_purge(Mod) of
      true -> false;
      false -> true
    end
        end,
  RestMods = lists:map(Fun, UnPurgedMods),
  case RestMods of
    [] -> ok;
    _ ->
      timer:sleep(1000),
      soft_purge(RestMods)
  end.
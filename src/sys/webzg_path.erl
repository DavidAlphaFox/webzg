%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 文件路径
%%% @end
%%% Created : 25. 三月 2016 15:20
%%%-------------------------------------------------------------------
-module(webzg_path).
-author("zhaogang").
-include("logger.hrl").
%% API
-export([project_dir/0,
  code_dir/0,
  priv_dir/0,
  path/1,
  webfile_dir/0,
  upload_dir/0,
  get_path/1,
  upload_file/1,
  upload_file/2, upload_file_url/3]).


%% lib所在路径
project_dir() ->
  filename:dirname(code:which(?MODULE)) ++ "/..".

%% priv路径
priv_dir() ->
  project_dir() ++ "/priv".

%% 代码所在路径
code_dir() ->
  project_dir() ++ "/ebin".

%% 网页所在路径
webfile_dir() ->
  priv_dir() ++ "/docroot".

upload_dir() ->
  webfile_dir() ++ "/upload".


%% 文件绝对路径
path(index) ->
  webfile_dir() ++ "/index.html";
path(cacertfile) ->
  priv_dir() ++ "/ssl/cowboy-ca.crt";
path(certfile) ->
  priv_dir() ++ "/ssl/server.crt";
path(keyfile) ->
  priv_dir() ++ "/ssl/server.key";
path(template) ->
  priv_dir() ++ "/templates".



%% @doc 只考虑linux路径下
get_path(FilePath) ->
  ModPath = string:tokens(code:which(?MODULE), "/"),
  {NewFilePath, N} = position_path(FilePath, 0),
  ModPathLen = length(ModPath),
  case os:type() of
    {win32, _} -> string:join(lists:sublist(ModPath, ModPathLen - 1 - N) ++ [NewFilePath], "/");
    _ -> "/" ++ string:join(lists:sublist(ModPath, ModPathLen - 1 - N) ++ [NewFilePath], "/")
  end.

%% (相对路径，../的层数)
-spec position_path(FilePath :: string(), N :: integer()) -> {FilePath :: string(), N :: integer()}.
position_path("../" ++ FilePath, N) -> position_path(FilePath, N + 1);
position_path(FilePath, N) -> {FilePath, N}.

%% 处理文件上传stream
upload_file(Bytes) ->
  upload_file(Bytes, <<>>).

upload_file(Bytes, ContentType) ->
  FileExtension = extension(ContentType),
  FileID = webzg_util:new_id(),
  UploadDir = upload_dir(),
  FileName = filename:join(UploadDir, <<FileID/binary, FileExtension/binary>>),
  ?DEBUG("~p ---> ~p~n", [<<FileID/binary, FileExtension/binary>>, FileName]),
  case file:open(FileName, [write, raw]) of
    {ok, S} ->
      %%ok = io:format(S, "~s", [Bytes]),
      ok = file:write(S, Bytes),
      file:close(S),
      {ok, filename:basename(FileName)};
    Error ->
      Error
  end.

upload_file_url(Host, Bytes, ContentType) ->
  {ok, FileName} = upload_file(Bytes, ContentType),
  iolist_to_binary([Host, "/files/upload/", FileName]).

%% content-type对应的扩展名
extension(<<"image/jpeg">>) ->
  <<".jpg">>;
extension(<<"image/gif">>) ->
  <<".gif">>;
extension(<<"image/svg+xml">>) ->
  <<".svg">>;
extension(<<"image/png">>) ->
  <<".png">>;
extension(<<"text/plain">>) ->
  <<>>;
extension(_) ->
  <<>>.

%%web_ext(<<"css">>) -> {<<"text">>, <<"css">>, []};
%%web_ext(<<"gif">>) -> {<<"image">>, <<"gif">>, []};
%%web_ext(<<"html">>) -> {<<"text">>, <<"html">>, []};
%%web_ext(<<"htm">>) -> {<<"text">>, <<"html">>, []};
%%web_ext(<<"ico">>) -> {<<"image">>, <<"x-icon">>, []};
%%web_ext(<<"jpeg">>) -> {<<"image">>, <<"jpeg">>, []};
%%web_ext(<<"jpg">>) -> {<<"image">>, <<"jpeg">>, []};
%%web_ext(<<"js">>) -> {<<"application">>, <<"javascript">>, []};
%%web_ext(<<"mp3">>) -> {<<"audio">>, <<"mpeg">>, []};
%%web_ext(<<"mp4">>) -> {<<"video">>, <<"mp4">>, []};
%%web_ext(<<"ogg">>) -> {<<"audio">>, <<"ogg">>, []};
%%web_ext(<<"ogv">>) -> {<<"video">>, <<"ogg">>, []};
%%web_ext(<<"png">>) -> {<<"image">>, <<"png">>, []};
%%web_ext(<<"svg">>) -> {<<"image">>, <<"svg+xml">>, []};
%%web_ext(<<"wav">>) -> {<<"audio">>, <<"x-wav">>, []};
%%web_ext(<<"webm">>) -> {<<"video">>, <<"webm">>, []};
%%web_ext(_) -> {<<"application">>, <<"octet-stream">>, []}.
%% Feel free to use, reuse and abuse the code in this file.

%% @doc Pastebin handler.
-module(file_handler).
-export([init/2]).
-export([malformed_request/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([last_modified/2]).
-export([generate_etag/2]).
-export([get_file/2, fullpath/1]).

-type extra_etag() :: {etag, module(), function()} | {etag, false}.
-type extra_mimetypes() :: {mimetypes, module(), function()}
| {mimetypes, binary() | {binary(), binary(), [{binary(), binary()}]}}.
-type extra() :: [extra_etag() | extra_mimetypes()].
-type opts() :: {file | dir, string() | binary()}
| {file | dir, string() | binary(), extra()}
| {priv_file | priv_dir, atom(), string() | binary()}
| {priv_file | priv_dir, atom(), string() | binary(), extra()}.
-export_type([opts/0]).

-include_lib("kernel/include/file.hrl").
-include("logger.hrl").

-type state() :: {binary(), {ok, #file_info{}} | {error, atom()}, extra()}.

%% Resolve the file that will be sent and get its file information.
%% If the handler is configured to manage a directory, check that the
%% requested file is inside the configured directory.

-spec init(Req, opts()) -> {cowboy_rest, Req, error | state()} when Req::cowboy_req:req().
%% http请求初始化
init(Req, {Name, Path}) ->
  init_opts(Req, {Name, Path, []});
init(Req, {Name, App, Path}) when Name =:= priv_file; Name =:= priv_dir ->
  init_opts(Req, {Name, App, Path, []});
init(Req, Opts) ->
  init_opts(Req, Opts).

%% App priv 路径下的文件
init_opts(Req, {priv_file, App, Path, Extra}) ->
  init_info(Req, absname(priv_path(App, Path)), Extra);
%% App priv 路径下的目录中的所有文件
init_opts(Req, {priv_dir, App, Path, Extra}) ->
  init_dir(Req, priv_path(App, Path), Extra);
%% 指定路径下的文件
init_opts(Req, {file, Path, Extra}) ->
  init_info(Req, absname(Path), Extra);
%% 指定路径下的目录中的所有文件
init_opts(Req, {dir, Path, Extra}) ->
  init_dir(Req, Path, Extra).

priv_path(App, Path) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      error({badarg, "Can't resolve the priv_dir of application "
        ++ atom_to_list(App)});
    PrivDir when is_list(Path) ->
      PrivDir ++ "/" ++ Path;
    PrivDir when is_binary(Path) ->
      << (list_to_binary(PrivDir))/binary, $/, Path/binary >>
  end.

absname(Path) when is_list(Path) ->
  filename:absname(list_to_binary(Path));
absname(Path) when is_binary(Path) ->
  filename:absname(Path).

init_dir(Req, Path, Extra) when is_list(Path) ->
  init_dir(Req, list_to_binary(Path), Extra);
init_dir(Req, Path, Extra) ->
  Dir = fullpath(filename:absname(Path)),
  PathInfo = cowboy_req:path_info(Req),
  Filepath = filename:join([Dir|PathInfo]), %% 根据Dir路径生成要访问的文件路径
  Len = byte_size(Dir),
  case fullpath(Filepath) of
    << Dir:Len/binary, $/, _/binary >> -> %% 确实要访问的文件路径是Dir目录下的文件
      init_info(Req, Filepath, Extra);
    _ ->
      {cowboy_rest, Req, error}
  end.

%% 获取真实绝对路径, 主要是将..转到父目录
fullpath(Path) ->
  fullpath(filename:split(Path), []).
fullpath([], Acc) ->
  filename:join(lists:reverse(Acc));
fullpath([<<".">>|Tail], Acc) ->
  fullpath(Tail, Acc);
%% 对最顶级路径..的操作无效,因为没有更高一层目录
fullpath([<<"..">>|Tail], Acc=[_]) ->
  fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], [_|Acc]) ->
  fullpath(Tail, Acc);
fullpath([Segment|Tail], Acc) ->
  fullpath(Tail, [Segment|Acc]).


%% 该static的state{Path, Info, Extra}
init_info(Req, Path, Extra) ->
  Info = file:read_file_info(Path, [{time, universal}]),
  {cowboy_rest, Req, {Path, Info, Extra}}.

%% Reject requests that tried to access a file outside
%% the target directory.

-spec malformed_request(Req, State)
      -> {boolean(), Req, State}.
malformed_request(Req, State) ->
  {State =:= error, Req, State}.


%% http authorization验证用户名密码
%%is_authorized(Req, State) ->
%%  ?DEBUG("~p~n~p~n~p", [Req, State, catch cowboy_req:parse_header(<<"authorization">>, Req)]),
%%  case account:check_auth(Req) of
%%    {true, _} ->
%%      {true, Req, State};
%%    _ ->
%%      case cowboy_req:parse_header(<<"authorization">>, Req) of
%%        {basic, User = <<"aaa">>, <<"123456">>} ->
%%          {true, Req, State};
%%        Else ->
%%          ?DEBUG("~p~n", [Else]),
%%          {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
%%      end
%%  end.



%% Directories, files that can't be accessed at all and
%% files with no read flag are forbidden.
%% 读取的文件信息显示该文件不可read,即forbidden
-spec forbidden(Req, State)
      -> {boolean(), Req, State}
  when State::state().
forbidden(Req, State={_, {ok, #file_info{type=directory}}, _}) ->
  {true, Req, State};
forbidden(Req, State={_, {error, eacces}, _}) ->
  {true, Req, State};
forbidden(Req, State={_, {ok, #file_info{access=Access}}, _})
  when Access =:= write; Access =:= none ->
  {true, Req, State};
forbidden(Req, State) ->
  %% 验证token是否有效,否则不允许访问文件
  case account:check_auth(Req) of
    {true, _User} ->
      {false, Req, State};
    _ ->
      Req2 = cowboy_req:set_resp_body(<<"error token">>, Req),
      {true, Req2, State}
  end.



%% Detect the mimetype of the file.
-spec content_types_provided(Req, State)
      -> {[{binary(), get_file}], Req, State}
  when State::state().
content_types_provided(Req, State={Path, _, Extra}) ->
  case lists:keyfind(mimetypes, 1, Extra) of
    false ->
      {[{cow_mimetypes:web(Path), get_file}], Req, State};
    {mimetypes, Module, Function} ->
      {[{Module:Function(Path), get_file}], Req, State};
    {mimetypes, Type} ->
      {[{Type, get_file}], Req, State}
  end.

%% Assume the resource doesn't exist if it's not a regular file.
-spec resource_exists(Req, State)
      -> {boolean(), Req, State}
  when State::state().
resource_exists(Req, State={_, {ok, #file_info{type=regular}}, _}) ->
  {true, Req, State};
resource_exists(Req, State) ->
  {false, Req, State}.

%% Generate an etag for the file.
-spec generate_etag(Req, State)
      -> {{strong | weak, binary()}, Req, State}
  when State::state().
generate_etag(Req, State={Path, {ok, #file_info{size=Size, mtime=Mtime}},
  Extra}) ->
  case lists:keyfind(etag, 1, Extra) of
    false ->
      {generate_default_etag(Size, Mtime), Req, State};
    {etag, Module, Function} ->
      {Module:Function(Path, Size, Mtime), Req, State};
    {etag, false} ->
      {undefined, Req, State}
  end.

generate_default_etag(Size, Mtime) ->
  {strong, integer_to_binary(erlang:phash2({Size, Mtime}, 16#ffffffff))}.

%% Return the time of last modification of the file.

-spec last_modified(Req, State)
      -> {calendar:datetime(), Req, State}
  when State::state().
last_modified(Req, State={_, {ok, #file_info{mtime=Modified}}, _}) ->
  {Modified, Req, State}.

%% Stream the file.
%% @todo Export cowboy_req:resp_body_fun()?

-spec get_file(Req, State)
      -> {{stream, non_neg_integer(), fun()}, Req, State}
  when State::state().
get_file(Req, State={Path, {ok, #file_info{size=Size}}, _}) ->
  Sendfile = fun (Socket, Transport) ->
    case Transport:sendfile(Socket, Path) of
      {ok, _} -> ok;
      {error, closed} -> ok;
      {error, etimedout} -> ok
    end
             end,
  {{stream, Size, Sendfile}, Req, State}.

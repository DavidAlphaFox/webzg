
%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 二月 2016 11:44
%%%-------------------------------------------------------------------
-module(upload_handler).
-author("zhaogang").
-include("logger.hrl").
-include("code.hrl").
-record(state, {user}).
%% 记录用户上传的文件
-include("upload.hrl").
-export([init/2, recv_upload/2, content_types_accepted/2, allowed_methods/2, forbidden/2]).

init(Req, Opts) ->
  random:seed(now()),
  {cowboy_rest, Req, []}.

%% 只允许post方法
allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

%% Directories, files that can't be accessed at all and
%% files with no read flag are forbidden.

forbidden(Req, State) ->
  %% 验证token是否有效,否则不允许访问文件
  case account:check_auth(Req) of
    {true, User} ->
      {false, Req, #state{user = User}};
    _ ->
      Req2 = cowboy_req:set_resp_body(<<"error token">>, Req),
      {true, Req2, State}
  end.

%% *表示允许任意类型的文件上传
content_types_accepted(Req, State) ->
  {[{'*', recv_upload}],
    Req, State}.

%% 处理upload
recv_upload(Req, State) ->
  {ok, Headers, Req2} = cowboy_req:part(Req),
  {ok, Data, Req3} = cowboy_req:part_body(Req2),
  ?DEBUG("~p~n", [cow_multipart:form_data(Headers)]),
  {file, <<"inputfile">>, _Filename, ContentType, _TE}
    = cow_multipart:form_data(Headers),
  ?DEBUG("~p~n", [ContentType]),
  FileUrl = webzg_path:upload_file_url(cowboy_req:host_url(Req), Data, ContentType),
  Record = record(State, FileUrl, get_desc(Req)),
  Req4 = write(Req3, Record),
  {true, Req4, State}.



record(State, FileName, Desc) ->
  R = #file_record{user = State#state.user,
    file = FileName,
    timestamp = lib_time:iso_time(),
    desc = Desc},
  mnesia:dirty_write(R),
  R.


get_desc(Req) ->
  QueryList = cowboy_req:parse_qs(Req),
  {_, Desc} = lists:keyfind(<<"desc">>, 1, QueryList),
  Desc.

write(Req, #file_record{user = _User,
  file = FileName,
  timestamp = Time}) ->
  Res = #{<<"code">> => ?CODE_SUCCESS,
    <<"data">> => #{<<"file">> => FileName,
                   <<"timestamp">> => Time}},
  cowboy_req:set_resp_body(jsx:encode(Res), Req).
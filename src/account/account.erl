%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 用户注册,登录,session等帐号逻辑控制模块
%%% @end
%%% Created : 04. 三月 2016 14:00
%%%-------------------------------------------------------------------
-module(account).
-author("zhaogang").


-include("account.hrl").
-include("code.hrl").
-export([start/0, login/1, session/1, auth/2, check_auth/1, check_token/2]).

start() ->
  ok.


%% 用户登录
login([User, Pass, IP]) ->
  case auth(User, Pass) of
    true ->
      {Token, Expire} = get_token(User, IP),
      case db_account:insert_session(User, {Token, Expire}, lib_time:timestamp()) of
        {result, ok} ->
          {Token, Expire};
        {error, _} ->
          {error, ?ERR_SERVER_ERR}
      end;
    false->
       {error, ?ERR_AUTH_FAILED}
  end.

session([]) ->
  func:term_to_string(ets:tab2list(session)).

%% 验证用户名密码
auth(User, Pass) ->
  <<User/binary, "pass">> == Pass.

%% 获取token
get_token(User, IP) ->
  Token = func:md5(iolist_to_binary([User, inet:ntoa(IP)])),
  {Token, ?EXPIRE_IN_SECONDS}.

%% 检查Token是否有效
check_token(Token, IP) ->
  case db_account:select_token(Token) of
    [] ->
      {false, ?TOKEN_ERROR};
    Session ->
      case get_token(Session#web_session.user, IP) of
        {Token, _} ->
          case check_token_expire(Session#web_session.expires_in, Session#web_session.time) of
            true -> {true, Session#web_session.user};
            false -> {false, ?TOKEN_EXPIRES}
          end;
        _ ->
          {false, ?TOKEN_ERROR}
      end
  end.

%% 检查Token是否过期
check_token_expire(Expires_in, TimeStamp) ->
  TimeStamp + Expires_in > lib_time:timestamp().

%% 检查是否有权限
check_auth(Req) ->
  Querys = cowboy_req:parse_qs(Req),
  Token = proplists:get_value(<<"token">>, Querys),
  {IP, _Port} = cowboy_req:peer(Req),
  account:check_token(Token, IP).
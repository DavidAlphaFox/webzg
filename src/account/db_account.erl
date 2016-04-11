%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 三月 2016 17:08
%%%-------------------------------------------------------------------
-module(db_account).
-author("zhaogang").

-include("account.hrl").
-include("code.hrl").
-export([init/1]).
-export([insert_user/2,
  insert_session/3,
  select_token/1
  ]).
%% API


init(_Opts) ->
  mnesia:create_table(web_session,
    [{ram_copies, [node()]},
      {attributes, record_info(fields, web_session)}]),
  mnesia:add_table_index(web_session, user),
  mnesia:add_table_copy(web_session, node(), ram_copies),
  mnesia:create_table(web_user,
    [{disc_copies, [node()]},
      {attributes, record_info(fields, web_user)}]),
  mnesia:add_table_copy(web_user, node(), disc_copies).



insert_user(UserName, Password) ->
  F = fun() ->
    mnesia:write(#web_user{username = UserName,
                       password = Password,
                       create_time = erlang:now()})
  end,
  mnesia:transaction(F).




insert_session(UserName, {Token, Expire}, Time) ->
  F = fun () ->
  mnesia:write(#web_session{token = Token, expires_in = Expire, user = UserName, time = Time})
  end,
  case catch mnesia:sync_dirty(F) of
    ok ->
      {result, ok};
    _ ->
      {error, ?ERR_MNESIA_EXCUTE_FAILED}
  end.

select_token(Token) ->
  case mnesia:dirty_read(web_session, Token) of
    [R] -> R;
    _ -> []
  end.


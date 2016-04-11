%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 三月 2016 16:38
%%%-------------------------------------------------------------------
-author("zhaogang").

-define(EXPIRE_IN_SECONDS, 7200).
-record(web_session, {token, time, user, expires_in}).
-record(web_user,   {username, password, create_time}).
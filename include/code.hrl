%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 三月 2016 19:25
%%%-------------------------------------------------------------------
-author("zhaogang").

-define(CODE_SUCCESS, <<"0">>).

%% error code
-define(ERR_SQL_EXCUTE_FAILED, <<"1">>).
-define(ERR_MNESIA_EXCUTE_FAILED, <<"2">>).
-define(ERR_AUTH_FAILED, <<"3">>).
-define(ERR_SERVER_ERR, <<"4">>).
-define(TOKEN_ERROR, <<"5">>).
-define(TOKEN_EXPIRES, <<"6">>).
-define(UNKNOWN_CMD, <<"7">>).
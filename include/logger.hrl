%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 二月 2016 15:16
%%%-------------------------------------------------------------------
-author("zhaogang").

-define(DEBUG(Args), lager:debug(Args)).
-define(DEBUG(Format, Args), lager:debug(Format, Args)).
-define(INFO_MSG(Args), lager:info(Args)).
-define(INFO_MSG(Format, Args), lager:info(Format, Args)).
-define(WARNING_MSG(Args), lager:warning(Args)).
-define(WARNING_MSG(Format, Args), lager:warning(Format, Args)).
-define(ERROR_MSG(Args), lager:error(Args)).
-define(ERROR_MSG(Format, Args), lager:error(Format, Args)).

-define(PR(Arg), lager:pr(Arg, cowboy_req)).

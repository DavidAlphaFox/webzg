%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 二月 2016 15:32
%%%-------------------------------------------------------------------
-module(cookie_handler).
-author("zhaogang").

-export([init/2]).

init(Req, Opts) ->
  NewValue = integer_to_list(random:uniform(1000000)),
  Req2 = cowboy_req:set_resp_cookie(
    <<"c2">>, NewValue, [{path, <<"/">>}], Req),
  Req3 = cowboy_req:set_resp_cookie(
    <<"client2">>, "test3", [{path, <<"/">>}], Req2),
  #{client := ClientCookie, server := ServerCookie}
    = cowboy_req:match_cookies([{client, [], <<>>}, {server, [], <<>>}], Req3),
  {ok, Body} = tpl_cookie:render([
    {client, ClientCookie},
    {server, ServerCookie}
  ]),
  Req4 = cowboy_req:reply(200,
    [{<<"content-type">>, <<"text/html">>}],
    Body, Req3),
  {ok, Req4, Opts}.

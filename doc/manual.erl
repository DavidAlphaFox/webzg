%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 三月 2016 15:50
%%%-------------------------------------------------------------------
-module(manual).
-author("zhaogang").

%% API
-export([]).

-record(http_req, {
  %% Transport.
  socket = undefined :: any(),
  transport = undefined :: undefined | module(),
  connection = keepalive :: keepalive | close,
  %% Request.
  pid = undefined :: pid(),
  method = <<"GET">> :: binary(),
  version = 'HTTP/1.1' :: cowboy:http_version(),
  peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
  host = undefined :: undefined | binary(),
  host_info = undefined :: undefined | cowboy_router:tokens(),
  port = undefined :: undefined | inet:port_number(),
  path = undefined :: binary(),
  path_info = undefined :: undefined | cowboy_router:tokens(),
  qs = undefined :: binary(),
  bindings = undefined :: undefined | cowboy_router:bindings(),
  headers = [] :: cowboy:http_headers(),
  meta = [] :: [{atom(), any()}],

  %% Request body.
  body_state = waiting :: waiting | done | {stream, non_neg_integer(),
    transfer_decode_fun(), any(), content_decode_fun()},
  buffer = <<>> :: binary(),
  multipart = undefined :: undefined | {binary(), binary()},

  %% Response.
  resp_compress = false :: boolean(),
  resp_state = waiting :: locked | waiting | waiting_stream
  | chunks | stream | done,
  resp_headers = [] :: cowboy:http_headers(),
  resp_body = <<>> :: iodata() | resp_body_fun()
  | {non_neg_integer(), resp_body_fun()}
  | {chunked, resp_chunked_fun()},

  %% Functions.
  onresponse = undefined :: undefined | already_called
  | cowboy:onresponse_fun()
}).

http_req_example() ->
#http_req{socket=#Port<0.4463>,
  transport=ranch_tcp,
  connection=keepalive,
  pid=pid(0,325,0),
  method= <<"GET">>,
  version='HTTP/1.1',
  peer={{192,168,10,80},53061},
  host= <<"innodealing-dev">>,
  host_info=undefined,
  port=8080,
  path= <<"/cgi-bin/token">>,
  path_info=[<<"token">>],
  qs= <<>>,
  bindings=[],
  headers=[{<<"host">>,<<"innodealing-dev:8080">>},
    {<<"connection">>,<<"keep-alive">>},
    {<<"cache-control">>,<<"max-age=0">>},
    {<<"accept">>,<<"text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8">>},
    {<<"upgrade-insecure-requests">>,<<"1">>},
    {<<"user-agent">>,<<"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36">>},
    {<<"accept-encoding">>,<<"gzip, deflate, sdch">>},
    {<<"accept-language">>,<<"zh-CN,zh;q=0.8,en;q=0.6">>}],
  meta=[],
  body_state=waiting,
  buffer= <<>>,
  multipart=undefined,
  resp_compress=false,
  resp_state=waiting,
  resp_headers=[],
  resp_body= <<>>,
  onresponse= #Fun<webzg_app.0.9777477>}.


req2() ->
  #http_req{socket=#Port<0.4463>,
    transport=ranch_tcp,
    connection=keepalive,
    pid=<0.325.0>,
    method=<<"GET">>,
    version='HTTP/1.1',
    peer={{192,168,10,80},53061},
    host=<<"innodealing-dev">>,
    host_info=undefined,
    port=8080,path=<<"/cgi-bin/token">>,
    path_info=[<<"token">>],
    qs=<<>>,
    bindings=[],
    headers=
    [{<<"host">>, <<"innodealing-dev:8080">>},
    {<<"connection">>, <<"keep-alive">>},
    {<<"cache-control">>, <<"max-age=0">>},
    {<<"accept">>, <<"text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8">>},
    {<<"upgrade-insecure-requests">>, <<"1">>},
    {<<"user-agent">>, <<"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36">>},
    {<<"accept-encoding">>, <<"gzip,deflate,sdch">>},
    {<<"accept-language">>, <<"zh-CN,zh;q=0.8,en;q=0.6">>}],
    meta=[{media_type, {<<"application">>, <<"json">>, []}},
    {charset, undefined}],
    body_state=waiting,
    buffer=<<>>,
    multipart=undefined,
    resp_compress=false,
    resp_state=waiting,
    resp_headers=[{<<"content-type">>, [<<"application">>, <<"/">>, <<"json">>, <<>>]}],
    resp_body=<<>>,
    onresponse=#Fun<webzg_app.0.9777477>}.
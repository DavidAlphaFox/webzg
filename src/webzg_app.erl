-module(webzg_app).


%% Application callbacks
-export([start/2, stop/1]).
-include("webzg.hrl").
-include("logger.hrl").
-define(websv, websv).
-define(websvssl, websvssl).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
    Port = 8080,
    PortSSL = 8443,
    webzg_db:init(StartArgs),
    Dispatch = cowboy_router:compile([
        {'_',
          [
            {"/api/upload/",        upload_handler,   []},
            {"/api/[...]", api_handler,      state_here},
            {"/cookie/[...]",   cookie_handler,   []},
            {"/rest/[...]",     rest_handler,     []},
            {"/files/[...]",    file_handler,   {dir, webzg_path:webfile_dir(),
                                      [{mimetypes, cow_mimetypes, all}]}},
            {"/",               cowboy_static,    {file, webzg_path:path(index)}}
          ]}]),
    {ok, _} = cowboy:start_http(?websv,
                                100,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]},
                                 {onresponse, fun error_hook/4}]),
    {ok, _} = cowboy:start_https(?websvssl, 100,
                                [{port, PortSSL},
                                  {cacertfile, webzg_path:path(cacertfile)},
                                  {certfile, webzg_path:path(certfile)},
                                  {keyfile, webzg_path:path(keyfile)}],
                                [{env, [{dispatch, Dispatch},
                                        {onresponse, fun error_hook/4}]}]),
    webzg_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(?websv),
    cowboy:stop_listener(?websvssl).




%% 错误回调
error_hook(404, Headers, <<>>, Req) ->
  Path = cowboy_req:path(Req),
  {ok, Body} = tpl_page404:render([{file, Path}]),
  Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
              {<<"content-length">>, integer_to_list(iolist_size(Body))}),
  Headers3 = lists:keyreplace(<<"content-type">>, 1, Headers2, {<<"content-type">>,"text/html"}),
  cowboy_req:reply(404, Headers3, Body, Req);
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
  Body = ["HTTP Error ", integer_to_list(Code), $\n],
  Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
    {<<"content-length">>, integer_to_list(iolist_size(Body))}),
  cowboy_req:reply(Code, Headers2, Body, Req);
error_hook(_Code, _Headers, _Body, Req) ->
  Req.





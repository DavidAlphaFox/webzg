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

-export([init/2]).

init(Req, Opts) ->
  {ok, Headers, Req2} = cowboy_req:part(Req),
  {ok, Data, Req3} = cowboy_req:part_body(Req2),
  {file, <<"inputfile">>, _Filename, _ContentType, _TE}
    = cow_multipart:form_data(Headers),
  FileID = webzg_util:new_id(),
  UploadDir = webzg_dir:upload_dir(),
  ok = webzg_util:write_file(filename:join(UploadDir, FileID), Data),
  cowboy_req:reply(200, [], FileID, Req3),
  {ok, Req3, Opts}.

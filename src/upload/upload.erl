%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 四月 2016 16:29
%%%-------------------------------------------------------------------
-module(upload).
-author("zhaogang").
-include("upload.hrl").
%% API
-export([init/1]).

init(_Opts) ->
  mnesia:create_table(file_record, [{disc_copies, [node()]}, {attributes, record_info(fields, file_record)}]),
  mnesia:add_table_index(file_record, #file_record.user).
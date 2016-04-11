-module(func).
-export([
  map_key_val/2,
  map_key_val/3,
  tocdata/1,
  escape_xml/1,
  dump_mnesia/1,
  dump_mnesia/2,
  md5/1,
  map_to_postkv/1,
  strip_blank/1,
  term_to_string/1]).




-spec md5(binary()) -> binary().
md5(Bin) ->
  iolist_to_binary([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(Bin))]).

%% get map attribute value
map_key_val(Map, Key) ->
  map_key_val(Map, Key, <<>>).

map_key_val(Map, Key, Default) ->
  case maps:find(Key, Map) of
    {ok, Val} ->
      Val;
    error ->
      Default
  end.

% module xml will transform CData to format: <<"<![CDATA[CData]]>">>
tocdata(CData) ->
  <<"TOCDATA", CData/binary>>.

escape_xml(null) ->
  <<>>;
escape_xml(Any) ->
  Any.

strip_blank(Binary) ->
  re:replace(Binary, "\\s+", "", [global,{return,binary}]).

%% 导出mnesia表
-spec dump_mnesia(any()) -> any().
dump_mnesia(Table) ->
  dump_mnesia(Table, "/tmp/mnesiadump_" ++ atom_to_list(Table) ++ lib_time:iso_time()).

dump_mnesia(Table, File) ->
  {ok, F} = file:open(File, [write]),
  W = mnesia:table_info(Table, wild_pattern),
  {atomic,All} = mnesia:transaction(
    fun() -> mnesia:match_object(Table, W, read) end),
  lists:foreach(
    fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, Table)]) end, All),
  file:close(F).

map_to_postkv(Map) ->
  [{HKey, HVal}|List] = maps:to_list(Map),
    lists:foldl(fun({Key, Val}, Bin)->
    <<Bin/binary, "&", Key/binary, "=", Val/binary>>
    end, <<HKey/binary, "=", HVal/binary>>, List).

%% term转为binary list
term_to_string(Term) ->
  iolist_to_binary(io_lib:format("~p", [Term])).
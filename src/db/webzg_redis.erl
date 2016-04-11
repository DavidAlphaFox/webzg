%%%-------------------------------------------------------------------
%%% @author zhaogang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 二月 2016 10:31
%%%-------------------------------------------------------------------
-module(webzg_redis).
-author("zhaogang").

%% API
-export([start/0]).
-define(PROCNAME, 'ejabberd_redis_client').
-define(MYHOSTS, [<<"webzg">>]).


start() ->
  Server = "192.168.8.98",
  Port = 6379,
  DB = 1,
  Pass = "",
  ReconnTimeout = 1,
  ConnTimeout = 1,
  case eredis:start_link(Server, Port, DB, Pass,
    ReconnTimeout, ConnTimeout) of
    {ok, Client} ->
      register(?PROCNAME, Client),
      %% clean_table(),
      ok;
    {error, _} = Err ->
      io:format("failed to start redis client: ~p", [Err]),
      Err
  end.


clean_table() ->
  lists:foreach(
    fun(LServer) ->
      ServKey = server_to_key(LServer),
      case eredis:q(?PROCNAME, ["HKEYS", ServKey]) of
        {ok, []} ->
          ok;
        {ok, Vals} ->
          Vals1 = lists:filter(
            fun(USSIDKey) ->
              {_, SID} = binary_to_term(USSIDKey),
              node(element(2, SID)) == node()
            end, Vals),
          Q1 = ["HDEL", ServKey | Vals1],
          Q2 = lists:map(
            fun(USSIDKey) ->
              {US, SID} = binary_to_term(USSIDKey),
              USKey = us_to_key(US),
              SIDKey = sid_to_key(SID),
              ["HDEL", USKey, SIDKey]
            end, Vals1),
          Res = eredis:qp(?PROCNAME, [Q1|Q2]),
          case lists:filter(
            fun({ok, _}) -> false;
              (_) -> true
            end, Res) of
            [] ->
              ok;
            Errs ->
              io:format("failed to clean redis table for "
              "server ~s: ~p", [LServer, Errs])
          end;
        Err ->
          io:format("failed to clean redis table for "
          "server ~s: ~p", [LServer, Err])
      end
    end, ?MYHOSTS).

test() ->
  {ok, <<"OK">>} = eredis:q(?PROCNAME, ["SET", "foo", "bar"]),
  {ok, <<"bar">>} = eredis:q(?PROCNAME, ["GET", "foo"]).



%%%===================================================================
%%% Internal functions
%%%===================================================================

iolist_to_list(IOList) ->
  binary_to_list(iolist_to_binary(IOList)).

us_to_key({LUser, LServer}) ->
  <<"ejabberd:sm:", LUser/binary, "@", LServer/binary>>.

server_to_key(LServer) ->
  <<"ejabberd:sm:", LServer/binary>>.

us_sid_to_key(US, SID) ->
  term_to_binary({US, SID}).

sid_to_key(SID) ->
  term_to_binary(SID).

decode_session_list([_, Val|T]) ->
  [binary_to_term(Val)|decode_session_list(T)];
decode_session_list([]) ->
  [].

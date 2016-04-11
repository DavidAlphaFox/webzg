#!/usr/bin/env escript
%%! -smp enable -sname erl-test1 -mnesia debug verbose

-define(set_table, set_table).
-define(ordered_set_table, ordered_set_table).

main([StrNum, StrType]) ->
    try
    N = list_to_integer(StrNum),
    Type = list_to_atom(StrType),
    init_table(),
    insert_data(N , Type)
  catch
    _:Error ->
      io:format("Error: ~p~n", [Error]),
      manual()
  end,
  exit();
main(_) ->
  manual().

manual() ->
  io:format("input data number and type eg:~n
  ./set-orderset-test.erl 100000 int ~n", []).


exit() ->
  halt(1).

init_table() ->
  ets:new(?set_table, [set, named_table, public]),
  ets:new(?ordered_set_table, [ordered_set, named_table, public]).


insert_data(N, Type) ->
  T1 = timer_start(),
  Keys = data(N, Type),
  F = fun(Table) ->
    lists:foreach(fun(Key) ->
      ets:insert(Table, {Key, 1})
      end, Keys) end,
  F2 = fun(Table) ->
    lists:foreach(fun(Key) ->
      ets:lookup(Table, Key)
                  end, Keys) end,
  timer_stop(T1, "process data"),
  {Time1, ok} = timer:tc(F,[?set_table]),
  {Time2, ok} = timer:tc(F,[?ordered_set_table]),
  {Time3, ok} = timer:tc(F2, [?set_table]),
  {Time4, ok} = timer:tc(F2,[?ordered_set_table]),
  io:format("~p data to ~p , insert time:~p read time ~p~n", [N, ?set_table, Time1, Time3]),
  io:format("~p data to ~p , insert time:~p read time ~p~n", [N, ?ordered_set_table, Time2, Time4]).


timer_start() ->
  erlang:now().

timer_stop(T1, Arg) ->
  io:format("~p cost:~p us~n", [Arg, timer:now_diff(erlang:now(), T1)]).

data(N, binary) ->
  lists:map(fun(_I) ->
    msgid()
  end, lists:seq(1, N));
data(N, int) ->
  lists:map(fun(I) ->
    I
  end, lists:seq(1, N)).

msgid() ->
  Initial = random:uniform(62) - 1,
  msgid(<<Initial>>, 7).
msgid(Bin, 0) ->
  Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
  << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
msgid(Bin, Rem) ->
  Next = random:uniform(62) - 1,
  msgid(<<Bin/binary, Next>>, Rem - 1).
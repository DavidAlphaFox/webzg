#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose

main([String]) ->
  try
    spawn(fun() -> print(String) end),
    N = list_to_integer(String),
    F = fac(N),
    io:format("factorial ~w = ~w~n", [N, F]),
    exit()
  catch
    _:Error ->
      io:format("Error: ~p~n", [Error]),
      manual()
  end;
main(_) ->
  manual().

manual() ->
  io:format("manual: factorial integer~n"),
  exit().


print(Arg) ->
  io:format("print===>~p~n", [Arg]).

exit() ->
  halt(1).

fac(0) -> 1;
fac(N) -> N * fac(N - 1).

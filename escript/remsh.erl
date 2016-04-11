#!/usr/bin/env escript
%%!-sname shremshejab@localhost   -smp enable


main([Module, Function|Args]) ->
  try
    io:format("~p~n", [nodes()]),
    {ok, HostName} = inet:gethostname(),
    EjabNode = list_to_atom("webzg@" ++ HostName),
    Res = rpc:call(EjabNode,list_to_atom(Module), list_to_atom(Function), Args),
    print([Res, nodes()]),
    write([Res, nodes()])
  catch
    _:Error ->
      io:format("Error: ~p~n", [Error]),
      manual()
  end,
  exit();
main(Arg) ->
  print(Arg),
  manual(),
  exit().

exit() ->
%%  io:format("press to exit~p~n", ["ctrl+c"]).
  ok.

manual() ->
  io:format("~p~n", ["ooooooooooooo"]).

print(Arg) ->
  io:format("~p~n", [Arg]).

write(Arg) ->
  file:write_file("/tmp/1.txt", io_lib:format("~p", [Arg])).
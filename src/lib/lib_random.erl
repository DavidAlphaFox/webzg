%%%----------------------------------------------------------------------
%%% File    : randoms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Random generation number wrapper
%%% Created : 13 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(lib_random).

-author('alexey@process-one.net').

-export([get_string/0, msgid/0]).

-export([start/0, init/0]).

start() ->
    register(random_generator, spawn(lib_random, init, [])).

init() ->
    {A1, A2, A3} = now(), random:seed(A1, A2, A3), loop().

loop() ->
    receive
      {From, get_random, N} ->
	  From ! {random, random:uniform(N)}, loop();
      _ -> loop()
    end.

%% 65536*65536 以内的随机数的binary
get_string() ->
    random_generator ! {self(), get_random, 4294967296},
    receive
      {random, R} -> list_to_binary(integer_to_list(R))
    end.


%% 随机id
msgid() ->
  Initial = random:uniform(62) - 1,
  msgid(<<Initial>>, 7).
msgid(Bin, 0) ->
  Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
  << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
msgid(Bin, Rem) ->
  Next = random:uniform(62) - 1,
  msgid(<<Bin/binary, Next>>, Rem - 1).
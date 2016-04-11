%% Feel free to use, reuse and abuse the code in this file.

%% @doc Pastebin handler.
-module(rest_test).

%% Standard callbacks.
-export([init/2]).
%%-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Custom callbacks.
%%-export([create_paste/2]).
-export([paste_html/2]).
-export([paste_text/2]).
-compile(export_all).
init(Req, Opts) ->
  random:seed(now()),
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

%%content_types_provided(Req, State) ->
%%  PasteId = case cowboy_req:binding(paste_id, Req) of
%%    undefined -> index;
%%    Val -> Val
%%  end,
%%  {[
%%%%    {{<<"application">>, <<"x-www-form-urlencoded">>, [PasteId]}, paste_text},
%%    {{<<"text">>, <<"html">>, [PasteId]}, paste_html},
%%    {{<<"text">>, <<"plain">>, [PasteId]}, paste_text}
%%  ], Req, State}.
content_types_provided(Req, State) ->
  {[
    {{<<"text">>, <<"plain">>, []}, paste_text},
    {{<<"text">>, <<"html">>, []}, paste_html}
  ], Req, State}.
content_types_accepted(Req, State) ->
  {[
    {'*', create_paste},
    {{<<"text">>, <<"plain">>, [{<<"charset">>, <<"utf-8">>}]},create_paste},
    {{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_paste}],
    Req, State}.

resource_exists(Req, _State) ->
  case cowboy_req:binding(paste_id, Req) of
    undefined ->
      {true, Req, index};
    PasteID ->
      io:format("~n111~p~n", [PasteID]),
      case valid_path(PasteID) and file_exists(PasteID) of
        true ->
          {true, Req, PasteID};
        false -> {false, Req, PasteID}
      end
  end.

create_paste(Req, State) ->
  PasteID = new_paste_id(),
  {ok, [{<<"paste">>, Paste}], Req2} = cowboy_req:body_qs(Req),
  ok = file:write_file(full_path(PasteID), Paste),
  case cowboy_req:method(Req2) of
    <<"POST">> ->
      {{true, <<$., $/, PasteID/binary>>}, Req2, State};
    _ ->
      {true, Req2, State}
  end.

paste_html(Req, index) ->
  io:format("~n~p~n", [index]),
  {read_file("index.html"), Req, index};
paste_html(Req, Paste) ->
  io:format("~n~p~n", [[?LINE, Paste]]),
  {format_html(Paste), Req, Paste}.

paste_text(Req, index) ->
  io:format("~n444~p~n", [index]),
  {read_file("index.txt"), Req, index};
paste_text(Req, Paste) ->
  io:format("~n444~p~n", [Paste]),
  #{lang := Lang} = cowboy_req:match_qs([lang], Req),
  {format_text(Paste, Lang), Req, Paste}.

% Private

read_file(Name) ->
  {ok, Binary} = file:read_file(full_path(Name)),
  Binary.

full_path(Name) ->
  io:format("~n2223 ~p ~p~n", [filename:join(app_path(), "priv"), Name]),
  filename:join([filename:join(app_path(), "priv"), Name]).


app_path() ->
  {ok, Path} = file:get_cwd(),
  Path.

file_exists(Name) ->
  case file:read_file_info(full_path(Name)) of
    {ok, _Info} -> true;
    {error, _Reason} -> false
  end.

valid_path(<<>>) -> true;
%%valid_path(<<$., _T/binary>>) -> false;
valid_path(<<$/, _T/binary>>) -> false;
valid_path(<<_Char, T/binary>>) -> valid_path(T).

new_paste_id() ->
  Initial = random:uniform(62) - 1,
  new_paste_id(<<Initial>>, 7).
new_paste_id(Bin, 0) ->
  Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
  << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
new_paste_id(Bin, Rem) ->
  Next = random:uniform(62) - 1,
  new_paste_id(<<Bin/binary, Next>>, Rem - 1).


format_html(Paste) ->
  Text = escape_html_chars(read_file(Paste)),
  <<"<!DOCTYPE html><html>",
    "<head><title>paste</title></head>",
    "<body><pre><code>", Text/binary, "</code></pre></body></html>\n">>.

format_html(Paste, plain) ->
  Text = escape_html_chars(read_file(Paste)),
  <<"<!DOCTYPE html><html>",
    "<head><title>paste</title></head>",
    "<body><pre><code>", Text/binary, "</code></pre></body></html>\n">>;
format_html(Paste, Lang) ->
  highlight(full_path(Paste), Lang, "html").

format_text(Paste, plain) ->
  read_file(Paste);
format_text(Paste, Lang) ->
  highlight(full_path(Paste), Lang, "ansi").

highlight(Path, Lang, Type) ->
  Path1 = binary_to_list(Path),
  Lang1 = binary_to_list(Lang),
  os:cmd(["highlight --syntax=", Lang1,
    " --doc-title=paste ",
    " --out-format=", Type,
    " --include-style ", Path1]).

% Escape some HTML characters that might make a fuss
escape_html_chars(Bin) ->
  << <<(escape_html_char(B))/binary>> || <<B>> <= Bin >>.

escape_html_char($<) -> <<"&lt;">>;
escape_html_char($>) -> <<"&gt;">>;
escape_html_char($&) -> <<"&amp;">>;
escape_html_char(C) -> <<C>>.

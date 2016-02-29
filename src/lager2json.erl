-module(lager2json).
-export([format/2, format/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


format(Msg, Config, _Colors) -> format(Msg, Config).

format(Msg, Config) ->
  JSON = jsx:encode(#{
    message => message(Msg),
    severity => severity(Msg),
    timestamp => timestamp(Msg),
    metadata => metadata(Msg)
  }),
  case Config of
    [{separator, Sep}] -> <<JSON/binary, Sep/binary>>;
    []                 -> JSON
  end.

message(Msg) ->
  unicode:characters_to_binary(lager_msg:message(Msg), unicode).

severity(Msg) ->
  lager_msg:severity(Msg).

timestamp(Msg) ->
  {MegaSec, Sec, MicroSec} = lager_msg:timestamp(Msg),
  USec = MegaSec * 1000000000000 + Sec * 1000000 + MicroSec,
  {ok, TimeStamp} = rfc3339:format(USec, micro_seconds),
  TimeStamp.

metadata(Msg) ->
  case lager_msg:metadata(Msg) of
    []   -> [{}];
    Else -> lists:map(fun printable/1, Else)
  end.

%% can't naively encode `File` or `Pid` as json as jsx see them as lists
%% of integers 
printable({file, File}) ->
  {file, unicode:characters_to_binary(File, unicode)};
printable({pid, Pid}) ->
  {pid, unicode:characters_to_binary(Pid, unicode)};
%% if a value is expressable in json use it directly, otherwise
%% try to get a printable representation and express it as a json
%% string
printable({Key, Value}) when is_atom(Key); is_binary(Key) ->
  case jsx:is_term(Value) of
    true  -> {Key, Value};
    false -> {Key, unicode:characters_to_binary(io_lib:format("~p", [Value]), unicode)}
  end.


-ifdef(TEST).

%% {{MegaSec, Sec, MicroSec}, <<"1970-01-01T00:00:00Z">>}
timestamp_now() ->
  Now = os:timestamp(),
  {MegaSec, Sec, MicroSec} = Now,
  {ok, TimeStamp} = rfc3339:format(
    MegaSec * 1000000000000 + Sec * 1000000 + MicroSec,
    micro_seconds
  ),
  {Now, TimeStamp}.

pid() ->
  Self = self(),
  {Self, unicode:characters_to_binary(io_lib:format("~p", [Self]), unicode)}.


format_test_() ->
  {Now, TimeStamp} = timestamp_now(),
  {Self, Pid} = pid(),
  [
    {"basic message", ?_assertEqual(
      <<"{\"message\":\"hallo world\",\"metadata\":{},\"severity\":\"info\",\"timestamp\":\"", TimeStamp/binary, "\"}">>,
      format(lager_msg:new("hallo world", Now, info, [], []), [])
    )},
    {"pid in metadata", ?_assertEqual(
      <<"{\"message\":\"hallo world\",\"metadata\":{\"pid\":\"", Pid/binary, "\"},\"severity\":\"info\",\"timestamp\":\"", TimeStamp/binary, "\"}">>,
      format(lager_msg:new("hallo world", Now, info, [{pid, io_lib:format("~p", [Self])}], []), [])
    )},
    {"file in metadata", ?_assertEqual(
      <<"{\"message\":\"hallo world\",\"metadata\":{\"file\":\"foo.erl\"},\"severity\":\"info\",\"timestamp\":\"", TimeStamp/binary, "\"}">>,
      format(lager_msg:new("hallo world", Now, info, [{file, "foo.erl"}], []), [])
    )},
    {"customizable separator", ?_assertEqual(
      <<"{\"message\":\"hallo world\",\"metadata\":{},\"severity\":\"info\",\"timestamp\":\"", TimeStamp/binary, "\"}\n----\n">>,
      format(lager_msg:new("hallo world", Now, info, [], []), [{separator, <<"\n----\n">>}])
    )}
  ].

-endif.
  
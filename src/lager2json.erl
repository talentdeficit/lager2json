-module(lager2json).
-export([format/2, format/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


format(Msg, Config, _Colors) -> format(Msg, Config).

format(Msg, Config) ->
  JSONMap = msg_to_map(Msg),
  JSONMap1 = add_static_config(JSONMap, proplists:get_value(static, Config)),
  JSON = jsx:encode(JSONMap1),
  case proplists:get_value(separator, Config) of
    undefined          -> JSON;
    Sep                -> <<JSON/binary, Sep/binary>>
  end.

add_static_config(JSONMap, undefined) ->
    JSONMap;
add_static_config(JSONMap, Static) ->
  JSONMap#{static => Static}.

msg_to_map(Msg) ->
  #{message => message(Msg),
    severity => severity(Msg),
    timestamp => timestamp(Msg),
    metadata => metadata(Msg)}.

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
    {pid, pid_list(Pid)};
%% if a value is expressable in json use it directly, otherwise
%% try to get a printable representation and express it as a json
%% string
printable({Key, Value}) when is_atom(Key); is_binary(Key) ->
  case jsx:is_term(Value) of
    true  -> {Key, Value};
    false -> {Key, unicode:characters_to_binary(io_lib:format("~p", [Value]), unicode)}
  end.

pid_list(Pid) ->
  try unicode:characters_to_binary(Pid, unicode) of
    Pid0 -> Pid0
    catch error:badarg ->
      unicode:characters_to_binary(hd(io_lib:format("~p", [Pid])), unicode)
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
   {"bare pid in metadata", ?_assertEqual(
                          <<"{\"message\":\"hallo world\",\"metadata\":{\"pid\":\"<0.6.0>\"},\"severity\":\"info\",\"timestamp\":\"", TimeStamp/binary, "\"}">>,
                          format(lager_msg:new("hallo world", Now, info, [{pid, list_to_pid("<0.6.0>")}], []), [])
     )},
    {"file in metadata", ?_assertEqual(
      <<"{\"message\":\"hallo world\",\"metadata\":{\"file\":\"foo.erl\"},\"severity\":\"info\",\"timestamp\":\"", TimeStamp/binary, "\"}">>,
      format(lager_msg:new("hallo world", Now, info, [{file, "foo.erl"}], []), [])
    )},
   {"static field", ?_assertEqual(
                       <<"{\"message\":\"hallo world\",\"metadata\":{},\"severity\":\"info\",\"static\":{\"app\":\"test\"},\"timestamp\":\"", TimeStamp/binary, "\"}">>,
      format(lager_msg:new("hallo world", Now, info, [], []), [{static, #{app => test}}])
    )},
    {"customizable separator", ?_assertEqual(
      <<"{\"message\":\"hallo world\",\"metadata\":{},\"severity\":\"info\",\"timestamp\":\"", TimeStamp/binary, "\"}\n----\n">>,
      format(lager_msg:new("hallo world", Now, info, [], []), [{separator, <<"\n----\n">>}])
    )}
  ].

-endif.

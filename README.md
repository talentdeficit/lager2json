# lager2json

a lager formatter that produces json

[![Build Status](https://travis-ci.org/talentdeficit/lager2json.svg?branch=master)](https://travis-ci.org/talentdeficit/lager2json)

* [using lager2json](#using-lager2json)
* [configuration](#configuration)
* [licensing and copyright](#licensing-and-copyright)

## using lager2json

when configuring *lager* specify `lager2json` as the formatter:
```erlang
{lager, [
  {handlers, [
    {lager_console_backend, [info, {lager2json, []}]},
    {lager_file_backend, [
      {file, "error.log"},
      {level, error},
      {formatter, lager2json},
      {formatter_config, []}
    ]},
  ]}
]}.
```
then call lager as you normally would:
```erlang
ok = lager:info("hallo world", []).
%% {"message":"hallo world","metadata":{"application":"lager2json","module":"some_module","function":"some_function","line":7,"pid":"<0.249.0>","node":"nonode@nohost"},"severity":"info","timestamp":"2016-02-29T00:38:44.954229Z"}
```

## configuration

*lager2json* supports the following configuration options

* `{separator, Separator}`
  append the separator (a binary) after the json output. useful when writing
  to the console or file

all configuration options should be passed as a proplist

## licensing and copyright

*lager2json* is available under either the MIT or Apache 2.0 licenses. see the
relevant `LICENSE-*` file included with this application

copyright 2016 alisdair sullivan

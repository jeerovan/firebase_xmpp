-module(firebase_xmpp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  ets:new(filesetting,[named_table,set,public,{read_concurrency,true},{write_concurrency,true}]),
  data_utils:restore_settings("../../settings.txt"),
  functions:check_settings(),
  application:ensure_started(mnesia),
	firebase_xmpp_sup:start_link().

stop(_State) ->
	ok.

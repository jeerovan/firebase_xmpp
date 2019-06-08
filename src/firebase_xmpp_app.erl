-module(firebase_xmpp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  ets_tables:create(),
  data_utils:restore_settings("../../settings.txt"),
  functions:check_settings(),
  Dispatch = cowboy_router:compile([
    {'_', [
           {"/stats", http_stat, []},
           {"/static/[...]", cowboy_static,
            {priv_dir, firebase_xmpp, "static",
               [{mimetypes, cow_mimetypes, all}]}
           },
           {"/ws", statsocket, []}
          ]}
    ]),
  Port = filesettings:get(application_port,33000),
  {ok,_} = cowboy:start_clear(http,
                    [{port, Port}],
                    #{
                     compress => true,
                     env => #{dispatch => Dispatch}}),
	firebase_xmpp_sup:start_link().

stop(_State) ->
	ok.

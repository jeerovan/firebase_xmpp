-module(freevideotalk_app).
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
            {priv_dir, freevideotalk, "static",
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
	freevideotalk_sup:start_link().

stop(_State) ->
	ok.

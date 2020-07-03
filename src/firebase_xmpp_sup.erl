-module(firebase_xmpp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [
            #{id => applog,
             start => {applog,start_link,[]},
             restart => permanent,
             type => worker,
             shutdown => 5000
            },
            #{id => fcm_socket,
             start => {fcm_socket,start_link,[]},
             restart => permanent,
             type => worker,
             shutdown => 5000
            }
          ],
	{ok, {{one_for_one, 1, 5}, Procs}}.

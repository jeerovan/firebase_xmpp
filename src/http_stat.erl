-module(http_stat).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  Method = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req),
  Reply = handle(Method,HasBody,Req),
  {ok,Reply,State}.

handle(_,_,Req) ->
  Template = bbmustache:parse_file(<<"../../priv/stat.html">>),
  HtmlBody = bbmustache:compile(Template,[]),
  cowboy_req:reply( 200,
                    #{},
                    HtmlBody,
                    Req).

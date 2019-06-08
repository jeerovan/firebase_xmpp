-module(statsocket).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state,{
                mode = text
              }
       ).

init(Req, _Opts) ->
	{cowboy_websocket, Req, #state{mode = text},#{idle_timeout => 30000}}.

websocket_init(State) ->
  gproc:reg({p,l,statsocket}),
	{ok, State}.

websocket_handle({binary, Data}, State) ->
  {Status,JSON} = msgpack:unpack(Data,[{unpack_str,as_binary}]),
  case Status of
    ok ->
      case maps:get(<<"mt">>,JSON,notfound) of
        notfound ->
          {ok,State};
        Mtype ->
          handle_mtype(Mtype,JSON,State#state{mode = binary})
      end;
    error ->
      self() ! {error,<<"Invalid JSON">>},
      {ok,State}
  end;
websocket_handle({text,Data},State) ->
  case jsx:is_json(Data) of
    true ->
      JSON = jsx:decode(Data,[return_maps]),
      case maps:get(<<"mt">>,JSON,notfound) of
        notfound ->
          {ok,State};
        Mtype ->
          handle_mtype(Mtype,JSON,State#state{mode = text})
      end;
    false ->
      self() ! {error,<<"Invalid JSON">>},
      {ok,State}
  end;
websocket_handle(_Frame, State) ->
	{ok, State}.

handle_mtype(0,_Data,State) ->
  gproc:send({p,l,processes},send_stats),
  {ok,State};
handle_mtype(_Mtype,_Data,State) ->
  {ok,State}.

websocket_info({fcm_process,Pid,Tasks},State) ->
  {reply,response(State,#{mt => 10, pid => list_to_binary(pid_to_list(Pid)), tasks => Tasks}),State};
websocket_info({fcm_manager,Pool,Pending},State) ->
  {reply,response(State,#{mt => 11, pool => Pool,pending => Pending}),State};
websocket_info({upstream_manager,Pool,Pending},State) ->
  {reply,response(State,#{mt => 12, receivers => Pool,pending => Pending}),State};
websocket_info({upstream,Pid,MqLen},State) ->
  {reply,response(State,#{mt => 13, pid => list_to_binary(pid_to_list(Pid)), mql => MqLen}),State};
websocket_info(_Info, State) ->
	{ok, State}.

terminate(_Reason,_Req,_State) ->
  ok.

response(State,Response) ->
  case State#state.mode of
    text ->
      {text,jsx:encode(Response)};
    binary ->
      {binary,msgpack:pack(Response,[{pack_str,from_binary}])}
  end.

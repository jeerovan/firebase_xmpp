-module(fcm_socket).
-behaviour(gen_statem).

%% API.
-export([start_link/0]).
-export([send_fcm/2]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([connecting/3]).
-export([connected/3]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
                socket
                }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_statem:start_link({local,?MODULE},?MODULE, [], []).
send_fcm(FcmId,Data) ->
  gen_statem:cast(?MODULE,{send,FcmId,Data}).

%% gen_statem.

callback_mode() ->
	state_functions.

init([]) ->
  process_flag(trap_exit, true),
  self() ! connect,
	{ok, connecting, #state{}}.

connecting(info,connect,State) ->
  applog:verbose(?MODULE,"Connecting~n",[]),
  UnixSocket = filesettings:get(unix_socket,"/tmp/fcm.socket"),
  case gen_tcp:connect({local, UnixSocket}, 0, [local,binary,{active,true},{packet,line},{buffer,4096}]) of
    {ok,Socket} ->
      {next_state,connected,State#state{socket = Socket}};
    _ ->
      {next_state,connecting,State,[{state_timeout,1000,connecting}]}
  end;
connecting(state_timeout,connecting,_State) ->
  self() ! connect,
  keep_state_and_data;
connecting(EventType,EventData,_State) ->
  applog:error(?MODULE,"Received Unhandled In Connecting:~p | ~p~n",[EventType,EventData]),
  keep_state_and_data.
connected(info,{tcp,_Socket,Data},_State) ->
  case jsx:is_json(Data) of
    true ->
      JsonData = jsx:decode(Data,[return_maps]),
      applog:verbose(?MODULE,"IN:~p~n",[JsonData]),
      #{<<"type">> := Type,<<"fcm_id">> := FcmId} = JsonData,
      case Type of
        <<"upstream_data">> ->
          #{<<"data">> := Json} = JsonData,
          incoming:data(FcmId,Json);
        <<"fcm_id_updated">> ->
          #{<<"old_fcm_id">> := OldFcmId} = JsonData,
          functions:update_fcm_id(OldFcmId,FcmId);
        <<"fcm_id_deregistered">> ->
          functions:device_unregistered(FcmId)
      end;
    false ->
      applog:error(?MODULE,"Received Invalid Json:~p~n",[Data])
  end,
  keep_state_and_data;
connected(info,{tcp_closed,_Socket},State) ->
  applog:verbose(?MODULE,"Socket Closed, Connecting~n",[]),
  {next_state,connecting,State,[{state_timeout,1000,connecting}]};
connected(info,{tcp_error,Socket,Reason},State) ->
  applog:error(?MODULE,"Tcp Error:~p~n",[Reason]),
  gen_tcp:close(Socket),
  {next_state,connecting,State,[{state_timeout,1000,connecting}]};
connected(cast,{send,FcmId,Data},#state{socket = Socket}) ->
  Json = #{<<"fcm_id">> => FcmId,<<"data">> => Data},
  JsonData = jsx:encode(Json),
  applog:verbose(?MODULE,"OUT:~p~n",[Json]),
  gen_tcp:send(Socket,<<JsonData/binary,"\n">>),
  keep_state_and_data;
connected(EventType, EventData, _State) ->
  applog:error(?MODULE,"Received Unhandled In Connected:~p | ~p~n",[EventType,EventData]),
  keep_state_and_data.

handle_event(_EventType, _EventData, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

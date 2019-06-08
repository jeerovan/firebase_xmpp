-module(upstream).
-include("constants.hrl").
-export([handler/1]).

handler(State) ->
  receive
    start ->
      gproc:reg({p,l,processes}),
      gproc:reg({p,l,upstream}),%---- Used To Limit Receiver And Stop When Manager Stops
      applog:info(?MODULE,"Started~n",[]),
      upstream_manager:add_upstream(self()),
      handler(State#{added => true});
    {process,AllData} ->
      #{upstream_task_upper_bound := UpperBound,
        upstream_task_lower_bound := LowerBound,
        added := Added} = State,
      {message_queue_len,MqLen} = process_info(self(),message_queue_len),
      NewState =
        case true of
          true when MqLen > UpperBound,Added =:= true ->
            upstream_manager:remove_upstream(self()),
            State#{added => false};
          true when MqLen < LowerBound,Added =:= false ->
            upstream_manager:add_upstream(self()),
            State#{added => true};
          true ->
            State
        end,
      #{<<"from">> := FcmId,<<"data">> := #{<<"data">> := JsonData}} = AllData,
      case jsx:is_json(JsonData) of
        true ->
          Json = jsx:decode(JsonData,[return_maps]),
          case maps:get(?messageType,Json,<<>>) of
            <<>> ->
              applog:error(?MODULE,"Received Message W/O Type:~p~n",[Json]);                                                                
            Type ->
              case maps:get(?userId,Json,<<>>) of
                <<>> ->
                  ok;
                UserId ->
                  functions:update_last_seen_at(UserId)
              end,
              applog:verbose('IN',"~p~n",[Json]),
              incoming:handle(Type,FcmId,Json)
          end;
        false ->
          applog:error(?MODULE,"Received Invalid Json From ~p~n",[FcmId])
      end,
      handler(NewState);
    send_stats ->
      {message_queue_len,Mqlen} = process_info(self(),message_queue_len),
      gproc:send({p,l,statsocket},{upstream,self(),Mqlen}),
      handler(State);
    {application_variable_update,Name,Value} ->
      NewState =
        case Name of
          upstream_task_upper_bound ->
            State#{ upstream_task_upper_bound => Value};
          upstream_task_lower_bound ->
            State#{ upstream_task_lower_bound => Value};
          _ ->
            State
        end,
      handler(NewState);
    stop ->
      applog:info(?MODULE,"Stopped~n",[])
  end.

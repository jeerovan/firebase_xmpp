-module(upstream).
-include("constants.hrl").
-export([handler/1]).

%---- Process to handle incoming data from mobile ----

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
      %---- JsonData Is Raw Data To Be Processed 
      %---- I Send It To Incoming Module
      %---- You May Process It Differently ------
      incoming:data(FcmId,JsonData),
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

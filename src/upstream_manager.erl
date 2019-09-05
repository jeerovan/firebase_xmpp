-module(upstream_manager).
-behaviour(gen_server).
-export([
         add_upstream/1,
         remove_upstream/1]).

%--- Manager to handle upstream processes to balance load --

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
                  upstream_counter = 0,
                  upstream_limit = 0,
                  upstream = 0
               }).

%% API.
%------ CALLS -----
%------ DIRECT APIS -----
add_upstream(Pid) ->
  case ets:member(upstream_id,Pid) of
    true ->
      applog:error(?MODULE,"Key Already Exists In MESSAGE RECEIVER ID~n",[]);
    false ->
      NextNumber = ets:update_counter(sequences,upstream_counter,{2,1},{upstream_counter,0}), 
      ets:insert(upstream_id,{Pid,NextNumber}),
      ets:insert(upstream,{NextNumber,Pid}),
      applog:info(?MODULE,"Added Message Receiver~n",[])
  end.

remove_upstream(Pid) ->
  case ets:take(upstream_id,Pid) of
    [{Pid,Key}] ->
      ets:delete(upstream,Key),
      applog:info(?MODULE,"Removed Message Receiver~n",[]);
    [] ->
      applog:error(?MODULE,"Key Does Not Exists In MESSAGE RECEIVER ID~n",[])
  end.

%------ CASTS -----

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

%% gen_server.

init([]) ->
  process_flag(trap_exit, true),
  gproc:reg({p,l,processes}),
  applog:info(?MODULE,"Started~n",[]),
  ProcessCount = length(gproc:select([{{{p,l,upstream},'$1','_'},[],['$1']}])),
  case ProcessCount of
    0 ->
      self() ! create_upstream;
    _ ->
      ok
  end,
  self() ! process_message,
  ReceiverProcessLimit = filesettings:get(upstream_limit,1),
	{ok, #state{upstream = ProcessCount,
              upstream_limit = ReceiverProcessLimit}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

try_with_first() ->
  case ets:first(upstream) of
    '$end_of_table' ->
      self() ! create_upstream,
      {0,no_one};
    First ->
      case ets:lookup(upstream,First) of
        [] ->
          {0,no_one};
        [{First,FPID}] ->
          case process_info(FPID,status) of
            undefined ->
              ets:delete(upstream,First),
              ets:delete(upstream_id,FPID),
              {0,no_one};
            _ ->
              {First,FPID}
          end
      end
  end.

handle_info(process_message,State) ->
  CurrentProcessCounter = State#state.upstream_counter,
  {NewProcessCounter,FPID} =
    case ets:info(upstream,size) > 0 of
      true ->
        case CurrentProcessCounter of
          0 ->
            try_with_first();
          CurrentProcessCounter ->
            case ets:next(upstream,CurrentProcessCounter) of
              '$end_of_table' ->
                try_with_first();
              Next ->
                case ets:lookup(upstream,Next) of
                  [] ->
                    {CurrentProcessCounter,no_one};
                  [{Next,NPID}] ->
                    case process_info(NPID,status) of
                      undefined ->
                        ets:delete(upstream,Next),
                        ets:delete(upstream_id,NPID),
                        {CurrentProcessCounter,no_one};
                      _ ->
                        {Next,NPID}
                    end
                end
            end
        end;
      false ->
        self() ! create_upstream,
        {0,no_one}
    end,
  NextProcessCounter =
    case FPID of
      no_one ->
        CurrentProcessCounter;
      _ ->
        case ets:first(incoming_message) of
          '$end_of_table' ->
            ets:delete(sequences,incoming_message_counter),
            timer:sleep(100),
            CurrentProcessCounter;
          First ->
            [{First,Data}] = ets:take(incoming_message,First),
            FPID ! {process,Data},
            NewProcessCounter
        end
    end,
  self() ! process_message,
  {noreply,State#state{upstream_counter = NextProcessCounter}};
handle_info(create_upstream,State) ->
  ReceiverProcessLimit = State#state.upstream_limit,
  ProcessCount = State#state.upstream,
  NewCount =
    case ProcessCount >= ReceiverProcessLimit of
      true ->
        ProcessCount;
      false ->
        UpperBound = filesettings:get(upstream_task_upper_bound,5000),
        LowerBound = filesettings:get(upstream_task_lower_bound,500),
        P = spawn(upstream,handler,[#{upstream_task_lower_bound => LowerBound,
                                      upstream_task_upper_bound => UpperBound}]),
        P ! start,
        ProcessCount + 1
    end,
  {noreply,State#state{upstream = NewCount}};
handle_info(send_stats,State) ->
  Receivers = ets:info(upstream,size),
  PendingMessages = ets:info(incoming_message,size),
  gproc:send({p,l,statsocket},{upstream_manager,Receivers,PendingMessages}),
  {noreply,State};
handle_info({application_variable_update,Name,Value},State) ->
  NewState =
    case Name of
      upstream_limit ->
        State#state{upstream_limit = Value};
      _ ->
        State
    end,
	{noreply, NewState};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
  [Process ! stop || Process <- gproc:select([{{{p,l,upstream},'$1','_'},[],['$1']}])].

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

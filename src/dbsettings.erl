-module(dbsettings).
-export([get/2,set/2,get_next_counter/1]).

get(Key,Default) ->
  case ets:info(dbsetting) of
    undefined ->
      Default;
    _ ->
      case ets:lookup(dbsetting,Key) of
        [] ->
          set(Key,Default),
          Default;
        [{dbsetting,Key,Value}] ->
          Value
      end
  end.
get_next_counter(Key) ->
  case ets:lookup(dbsetting,Key) of
    [] ->
      set(Key,1),
      1;
    [{dbsetting,Key,Value}] ->
      set(Key,Value + 1),
      Value + 1
  end.
set(Key,Value) ->
  [Process ! {application_variable_update,Key,Value} || Process <- gproc:select([{{{p,l,processes},'$1','_'},[],['$1']}])],
  db:write_record({dbsetting,Key,Value}).

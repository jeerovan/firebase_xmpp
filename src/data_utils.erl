-module(data_utils).
-compile([export_all,nowarn_export_all]).

%---- Some Random Data Utils ---

dump_table(Table,FileName) ->
  Records = ets:tab2list(Table),
  write_terms(FileName,Records).
restore_settings(FileName) ->
  Records = 
    case file:consult(FileName) of
      {error,_} ->
        [];
      {ok,Lines} ->
        Lines
    end,
  [ets:insert(filesetting,Record) || Record <- Records].
restore_table(FileName) ->
  {ok,Records} = file:consult(FileName),
  [datastore:write_record(Record) || Record <- Records].
%--------- to be used with file:consult -----------
write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~p.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).

%---- gives : {Value,[]} or {error,Reason}
to_float(Value) when is_float(Value) ->
  {Value,""};
to_float(Value) when is_binary(Value) ->
  ListValue = binary_to_list(Value),
  string:to_float(ListValue);
to_float(Value) when is_integer(Value) ->
  {error,"Only Floats."};
to_float(Value) when is_list(Value) ->
  string:to_float(Value).

%---- gives : {Value,[]} or {error,Reason}
to_integer(Value) when is_integer(Value) ->
  {Value,""};
to_integer(Value) when is_binary(Value) ->
  ListValue = binary_to_list(Value),
  string:to_integer(ListValue);
to_integer(Value) when is_float(Value) ->
  {error,"Only Integers."};
to_integer(Value) when is_list(Value) ->
  string:to_integer(Value).

fetch_integer(Key,Map,Default) ->
  Value = maps:get(Key,Map,<<>>),
  case Value of
    <<>> ->
      Default;
    Value ->
      case to_integer(Value) of
        {error,_Reason} ->
          Default;
        {Integer,_Reason} ->
          Integer
      end
  end.

fetch_float(Key,Map,Default) ->
  Value = maps:get(Key,Map,<<>>),
  case Value of
    <<>> ->
      Default;
    Value ->
      case to_float(Value) of
        {error,_Reason} ->
          Default;
        {Float,_Reason} ->
          Float
      end
  end.

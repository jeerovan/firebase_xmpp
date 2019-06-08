-module(data_utils).
-compile([export_all,nowarn_export_all]).

random_string(Len) ->
  Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(rand:uniform(ChrsSize), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, Len)).

format_multipart_formdata(Boundary,Fields, Files) ->
    FieldParts = lists:map(fun({FieldName, FieldContent}) ->
                                   [lists:concat(["--", Boundary]),
                                    lists:concat(["Content-Disposition: form-data; name=\"",FieldName,"\""]),
                                    "",
                                    FieldContent]
                           end, Fields),
    FieldParts2 = lists:append(FieldParts),
    FileParts = lists:map(fun({FieldName, FileName, FileContent}) ->
                                  [lists:concat(["--", Boundary]),
                                   lists:concat(["Content-Disposition: form-data; name=\"",FieldName,"\"; filename=\"",FileName,"\""]),
                                   lists:concat(["Content-Type: ", "application/octet-stream"]),
                                   "",
                                   FileContent]
                          end, Files),
    FileParts2 = lists:append(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
    string:join(Parts, "\r\n").

response_to_binary(Json) when is_binary(Json) ->
    Json;
response_to_binary(Json) when is_list(Json) ->
    list_to_binary(Json).

base64_decode(Token) when is_binary(Token)->
  base64_decode(binary_to_list(Token));
base64_decode(Token) ->
  Rem = length(Token) rem 4,
  NewToken = append_equal_to(4 - Rem,Token),
  base64:decode([case X of $_ -> $/; $- -> $+; _ -> X end || X <- NewToken]).

append_equal_to(0,Token) ->
  Token;
append_equal_to(Iter,Token) ->
  append_equal_to(Iter-1,Token ++ "=").

decode_base64(Base64) when is_list(Base64) ->
  try base64:decode(Base64)
  catch
    error:_ -> % could be missing =
      try base64:decode(Base64 ++ "=")
      catch
        error:_ -> % could be missing ==
        try base64:decode(Base64 ++ "==")
        catch
          error:_ -> % base64 is really wrong. we cannot fix it
            error
        end
      end
  end.

save_logs_to_file(Filename) ->
  Records = ets:tab2list(logs),
  List = [Line || {Line} <- Records],
  file:write_file(Filename,List,[append]),
  ets:delete_all_objects(logs).

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
    Format = fun(Term) -> io_lib:format("~w.~n", [Term]) end,
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

valid_lookup(Email) ->
  [_Name,Domain] = string:tokens(Email,"@"),
  MxList = inet_res:lookup(Domain,in,mx),
  case MxList of
    [] ->
      false;
    _ ->
      true
  end.

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

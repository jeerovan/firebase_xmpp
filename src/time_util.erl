-module(time_util).
-compile([export_all,nowarn_export_all]).

%--- erlang:system_time() gives the same result as below but not intended to get universal_time. It may change in the future depending on the implementation of what to get in system time. So be explicit.
%------ calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) = 62167219200

offset_to_seconds(Hours,Minutes) ->
  calendar:time_to_seconds({Hours,Minutes,0}).

utc_seconds() ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200.

utc_micro_seconds() ->
  {_Mega,_Seconds,MicroSeconds} = erlang:timestamp(),
  utc_seconds() * 1000000 + MicroSeconds.

old_utc_micro_seconds() ->
  erlang:system_time(micro_seconds).

local_seconds(OffsetSeconds) ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + OffsetSeconds - 62167219200.

local_seconds_from_utc_seconds(UTCSeconds,OffsetSeconds) ->
  UTCSeconds + OffsetSeconds.

utc_datetime(UTCSeconds) ->
  calendar:gregorian_seconds_to_datetime(UTCSeconds + 62167219200).

local_datetime(LocalSeconds) ->
  calendar:gregorian_seconds_to_datetime(LocalSeconds + 62167219200).

utc_seconds_from_utc_datetime(UTCDatetime) ->
  calendar:datetime_to_gregorian_seconds(UTCDatetime) - 62167219200.

local_seconds_from_local_datetime(LocalDatetime) ->
  calendar:datetime_to_gregorian_seconds(LocalDatetime) - 62167219200.

utc_string_from_utc_seconds(UTCSeconds) ->
  {{Y,Mo,D}, {H,Mn,S}} = utc_datetime(UTCSeconds),
  FmtStr = "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
  list_to_binary(IsoStr).

time_now_utc() ->
  {{Y,Mo,D}, {H,Mn,S}} = calendar:universal_time(),
  FmtStr = "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
  binary_to_list(list_to_binary(IsoStr)).

date_today_utc_string_bin() ->
  {{Y,Mo,D}, {_H,_Mn,_S}} = calendar:universal_time(),
  FmtStr = "~.4.0w-~.2.0w-~.2.0w",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D]),
  list_to_binary(IsoStr).

date_today_local_string_bin(Offset) ->
  {{Y,Mo,D}, {_H,_Mn,_S}} = local_datetime(local_seconds(Offset)),
  FmtStr = "~.4.0w-~.2.0w-~.2.0w",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D]),
  list_to_binary(IsoStr).

date_local_from_utc_seconds(UTCSeconds,OffsetSeconds) ->
  {{Y,Mo,D}, {_H,_Mn,_S}} = local_datetime(local_seconds_from_utc_seconds(UTCSeconds,OffsetSeconds)),
  FmtStr = "~.4.0w-~.2.0w-~.2.0w",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D]),
  list_to_binary(IsoStr).

time_now_local_string_bin(Offset) ->
  {{Y,Mo,D}, {H,Mn,S}} = local_datetime(local_seconds(Offset)),
  FmtStr = "~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
  IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
  list_to_binary(IsoStr).

check_iso_string(String) ->
  {ok,MP} = re:compile("(?:(?m)^(?:[1-9]\\d{3,3}\\-(?:(?:0[1-9]|1[0-2])\\-(?:0[1-9]|1\\d|2[0-8])|(?:0[13-9]|1[0-2])\\-(?:29|30)|(?:0[13578]|1[02])\\-31)|(?:[1-9]\\d(?:0[48]|[2468][048]|[13579][26])|(?:[2468][048]|[13579][26])00)\\-02\\-29)T(?:[01]\\d|2[0-3]):[0-5]\\d:[0-5]\\d(?:Z|[\\+\\-][01]\\d:[0-5]\\d)$)"),
  Result = re:run(String,MP),
  case Result of
    {match,[{0,20}]} ->
      match;
    {match,_} ->
      halfmatch;
    nomatch ->
      nomatch
  end.

get_seconds_from_iso_string(String) ->
  Year = list_to_integer(string:substr(String,1,4)),
  Month = list_to_integer(string:substr(String,6,2)),
  Day = list_to_integer(string:substr(String,9,2)),
  Hour = list_to_integer(string:substr(String,12,2)),
  Minute = list_to_integer(string:substr(String,15,2)),
  Second = list_to_integer(string:substr(String,18,2)),
  DateTime = {{Year,Month,Day},{Hour,Minute,Second}},
  Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
  Seconds.

has_request_expired(String) ->
  RequestSeconds = get_seconds_from_iso_string(String),
  CurrentSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  ExpirySeconds = functions:get_setting(request_expiry_seconds,30),
  case CurrentSeconds - RequestSeconds > ExpirySeconds of
    true ->
      yes;
    false ->
      no
  end.

daystime_to_seconds({Days,{Hour,Minute,Seconds}}) ->
  (Days * 24 * 3600) + (Hour * 3600) + (Minute * 60) + Seconds.

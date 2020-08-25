-module(aws_s3).
-compile([export_all,nowarn_export_all]).
-include("models.hrl").
-include("constants.hrl").

get_s3_post_params(UUID) ->
  DateStr = iso_8601_date_time(),
  ExpirySeconds = filesettings:get(s3_params_expiry_seconds,60),
  S3Host = list_to_binary(filesettings:get(s3_host,"")),
  Bucket = list_to_binary(filesettings:get(s3_bucket,"")),
  Expiration = list_to_binary(iso_8601_date_time_formatted(ExpirySeconds)),
  Credential = list_to_binary(credential_scope(DateStr)),
  DateBin = list_to_binary(DateStr),
  JSON = #{
            <<"expiration">> => Expiration,
            <<"conditions">> => [
                #{<<"acl">> => <<"public-read">>},
                #{<<"bucket">> => Bucket},
                %----[<<"starts-with">>, <<"$Content-Type">>, MimeTypeBin],
                #{<<"key">> => UUID},
                #{<<"x-amz-meta-uuid">> => UUID},
                #{<<"x-amz-algorithm">> => <<"AWS4-HMAC-SHA256">>},
                #{<<"x-amz-credential">> => Credential},
                #{<<"x-amz-date">> => DateBin}
              ]
            },
  Policy = base64:encode(jsx:encode(JSON)),
  Signature = signature(binary_to_list(Policy),DateStr),
  #{
    ?s3Url => <<"https://",S3Host/binary>>,
    ?s3Key => UUID,
    %---- Content-Type will be sent from client ---
    ?s3Acl => <<"public-read">>,
    ?s3MetaUuid => UUID,
    ?s3Bucket => Bucket,
    ?s3Policy => Policy,
    ?s3Signature => list_to_binary(Signature),
    ?s3Credential => Credential,
    ?s3Algorithm => <<"AWS4-HMAC-SHA256">>,
    ?s3Date => DateBin}.

iso_8601_date_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).
iso_8601_date_time_formatted(OffsetSeconds) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = time_util:utc_datetime(time_util:utc_seconds() + OffsetSeconds),
    lists:flatten(io_lib:format(
                    "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

base16(Data) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(Data)]).

credential_scope(Date) ->
  AccessId = filesettings:get(s3_access_id,""),
  Region = filesettings:get(s3_region,""),
  DateOnly = string:left(Date, 8),
  [AccessId,$/,DateOnly,$/,Region,"/s3/aws4_request"].

signing_key(Date) ->
  SecretKey = filesettings:get(s3_secret_key,""),
  Region = filesettings:get(s3_region,""),
  DateOnly = string:left(Date, 8),
  KDate = sha256_mac( "AWS4" ++ SecretKey, DateOnly),
  KRegion = sha256_mac( KDate, Region),
  KService = sha256_mac( KRegion, "s3"),
  sha256_mac( KService, "aws4_request").

signature(Policy,Date) ->
  base16(sha256_mac(signing_key(Date),Policy)).

sha256_mac(K, S) ->
    try
        crypto:mac(hmac,sha256, K, S)
    catch
        error:undef ->
            R0 = crypto:mac_init(hmac,sha256, K),
            R1 = crypto:mac_update(R0, S),
            crypto:mac_final(R1)
    end.
delete_media_file(Id) ->
  Bucket = list_to_binary(filesettings:get(s3_bucket,"")),
  Cmd = "aws s3 rm s3://" ++ binary_to_list(Bucket) ++ "/" ++ binary_to_list(Id),
  _ = spawn(os,cmd,[Cmd]).

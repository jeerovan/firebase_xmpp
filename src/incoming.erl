-module(incoming).
-include("constants.hrl").
-export([data/2]).

data(FcmId,JsonData) ->
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
          handle(Type,FcmId,Json)
      end;
    false ->
      applog:error(?MODULE,"Received Invalid Json From ~p~n",[FcmId])
  end.

handle(?getAppSettings,FcmId,Json) ->
  case maps:get(?userId,Json,<<>>) of
    <<>> ->
      ok;
    UserId ->
      functions:get_active_user_profiles(UserId,FcmId)
  end,
  Settings = #{?messageType => ?getAppSettings,
               ?responseStatus => ?successResponse,
               ?generalRequestTimeoutMillis => integer_to_binary(filesettings:get(generalRequestTimeoutMillis,5000)),
               ?s3Bucket => list_to_binary(filesettings:get(s3_bucket,"kaarsstest")),
               ?s3ParamsExpirySeconds => filesettings:get(s3_params_expiry_seconds,60)
              },
  downstream:create(FcmId,Settings);
handle(?userSignUp,FcmId,Json) ->
  Username = maps:get(?userName,Json,<<>>),
  Password = maps:get(?passWord,Json,<<>>),
  Country = maps:get(?country,Json,<<>>),
  case true of
    true when Username =:= <<>>; Password =:= <<>>; Country =:= <<>> ->
      Error = #{ ?messageType => ?userSignUp,
                 ?responseStatus => ?errorInResponse,
                 ?errorField => ?generalError,
                 ?responseError => ?parametersMissing},
      downstream:create(FcmId,Error);
    true ->
      %------ Get User State With Id And Other Params ------
      Response = functions:sign_up(Username,Password,Country,FcmId),
      downstream:create(FcmId,Response)
  end;
handle(?userSignIn,FcmId,Json) ->
  Username = maps:get(?userName,Json,<<>>),
  Password = maps:get(?passWord,Json,<<>>),
  case true of
    true when Username =:= <<>>; Password =:= <<>> ->
      Error = #{ ?messageType => ?userSignIn,
                 ?responseStatus => ?errorInResponse,
                 ?errorField => ?generalError,
                 ?responseError => ?parametersMissing},
      downstream:create(FcmId,Error);
    true ->
      Response = functions:sign_in(Username,Password,FcmId),
      downstream:create(FcmId,Response)
  end;
handle(?setSecurityAnswers,FcmId,Json) ->
  UserId = maps:get(?userId,Json,<<>>),
  One = maps:get(?answerOne,Json,<<>>),
  Two = maps:get(?answerTwo,Json,<<>>),
  Three = maps:get(?answerThree,Json,<<>>),
  Four = maps:get(?answerFour,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when UserId =:= <<>>; One =:= <<>>; Two =:= <<>>; Three =:= <<>>; Four =:= <<>>;TaskId =:= <<>> ->
      ok;
    true ->
      functions:set_security_questions(UserId,One,Two,Three,Four),
      functions:send_task_receipt(FcmId,TaskId)
  end;
handle(?changePassword,FcmId,Json) ->
  UserId = maps:get(?userId,Json,<<>>),
  Password = maps:get(?passWord,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when UserId =:= <<>>; Password =:= <<>>; TaskId =:= <<>> ->
      ok;
    true ->
      functions:change_password(UserId,Password),
      functions:send_task_receipt(FcmId,TaskId)
  end;
handle(?verifySecurityAnswers,FcmId,Json) ->
  UserName = maps:get(?userName,Json,<<>>),
  One = maps:get(?answerOne,Json,<<>>),
  Two = maps:get(?answerTwo,Json,<<>>),
  Three = maps:get(?answerThree,Json,<<>>),
  Four = maps:get(?answerFour,Json,<<>>),
  case true of
    true when UserName =:= <<>>; One =:= <<>>; Two =:= <<>>; Three =:= <<>>; Four =:= <<>> ->
      Error = #{ ?messageType => ?verifySecurityAnswers,
                 ?responseStatus => ?errorInResponse,
                 ?responseError => ?parametersMissing},
      downstream:create(FcmId,Error);
    true ->
      Response = functions:verify_security_questions(FcmId,UserName,One,Two,Three,Four),
      downstream:create(FcmId,Response)
  end;
handle(?getS3ParamsForDp,FcmId,Json) ->
  case maps:get(?userId,Json,<<>>)  of
    <<>> ->
      Error = #{ ?messageType => ?getS3ParamsForDp,
                 ?responseStatus => ?errorInResponse,
                 ?responseError => ?parametersMissing},
      downstream:create(FcmId,Error);
    UserId ->
      Response = functions:get_s3_params_for_dp(UserId,FcmId),
      downstream:create(FcmId,Response)
  end;
handle(?updateFcmId,FcmId,Json) ->
  UserId = maps:get(?userId,Json,<<>>),
  OldFcmId = maps:get(?oldFcmId,Json,<<>>),
  NewFcmId = maps:get(?newFcmId,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when TaskId =:= <<>>; UserId =:= <<>>; OldFcmId =:= <<>>; NewFcmId =:= <<>> ->
      ok;
    true ->
      functions:update_fcm_id(UserId,OldFcmId,NewFcmId),
      functions:send_task_receipt(FcmId,TaskId)
  end;
handle(?updateAgeGenderBio,FcmId,Json) ->
  UserId = maps:get(?userId,Json,<<>>),
  Age = data_utils:fetch_integer(?userAge,Json,-1),
  Gender = data_utils:fetch_integer(?userGender,Json,-1),
  Bio = maps:get(?userBio,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when TaskId =:= <<>>; UserId =:= <<>>; Age =:= -1; Gender =:= -1 ->
      ok;
    true ->
      functions:update_age_gender_bio(UserId,Age,Gender,Bio),                                                                    
      functions:send_task_receipt(FcmId,TaskId)
  end;
handle(?updateDpVersion,FcmId,Json) ->
  UserId = maps:get(?userId,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when TaskId =:= <<>>; UserId =:= <<>> ->
      ok;
    true ->
      functions:update_dp_version(UserId),
      functions:send_task_receipt(FcmId,TaskId)
  end;
handle(?updateUserBio,FcmId,Json) ->
  UserId = maps:get(?userId,Json,<<>>),
  Bio = maps:get(?userBio,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when TaskId =:= <<>>; UserId =:= <<>>; Bio =:= <<>> ->
      ok;
    true ->
      functions:update_user_bio(UserId,Bio),
      functions:send_task_receipt(FcmId,TaskId)
  end;
handle(?updateProfiles,FcmId,Json) ->
  case maps:get(?profilesData,Json,<<>>) of
    <<>> ->
      ok;
    ProfilesData ->
      functions:send_updated_profiles(FcmId,ProfilesData)
  end;
handle(?pendingTasks,_FcmId,Json) ->
  case maps:get(?userId,Json,<<>>) of
    <<>> ->
      ok;
    UserId ->
      functions:get_pending_tasks(UserId)
  end;
handle(?taskReceipt,_FcmId,Json) ->
  UserId = maps:get(?userId,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when UserId =:= <<>>;TaskId =:= <<>> ->
      ok;
    true ->
      functions:handle_task_receipt(UserId,TaskId)
  end;
handle(?fetchLastSeenAt,_FcmId,Json) ->
  FromUserId = maps:get(?userId,Json,<<>>),
  UserId = maps:get(?toUserId,Json,<<>>),
  case true of
    true when FromUserId =:= <<>>;UserId =:= <<>> ->
      ok;
    true ->
      functions:fetch_last_seen_at(FromUserId,UserId)
  end;
handle(?sendChatMessage,FcmId,Json) ->
  FromUserId = maps:get(?userId,Json,<<>>),
  ToUserId = maps:get(?toUserId,Json,<<>>),
  ChatMessage = maps:get(?chatMessage,Json,<<>>),
  ChatMessageId = maps:get(?chatMessageId,Json,<<>>),
  ChatType = data_utils:fetch_integer(?chatMessageType,Json,-1),
  TaskId = maps:get(?taskId,Json,<<>>),
  %--- Media Params ------
  MediaName = maps:get(?mediaName,Json,<<>>),
  InvalidMediaName = MediaName =:= <<>>,
  MediaSize = data_utils:fetch_integer(?mediaSize,Json,-1),
  InvalidMediaSize = mediaSize =:= -1,
  MediaData = maps:get(?mediaData,Json,<<>>),
  IsMediaType = lists:member(ChatType,[2,3,4,5,6]),
  case true of
    %---- General Conditions -----
    true when FromUserId =:= <<>>;
              ToUserId =:= <<>>;
              TaskId =:= <<>>;
              ChatMessageId =:= <<>>;
              ChatType =:= -1 ->
      ok;
    %---- No Text Message For Chat Type Text ---
    true when ChatMessage =:= <<>>,ChatType =:= 1 ->
      ok;
    %---- No Media Name/Size For Chat Type Media ----
    true when IsMediaType andalso InvalidMediaName or InvalidMediaSize ->
      ok;
    %---- Process ---
    true ->
      ChatMessageSentAt = #{ ?messageType => ?chatMessageSentAt,
                             ?responseStatus => ?successResponse,
                             ?chatMessageId => ChatMessageId,
                             ?taskId => TaskId,
                             ?timeStamp => time_util:utc_seconds()},
      downstream:create(FcmId,ChatMessageSentAt),
      functions:send_chat_message(FromUserId,
                                  ToUserId,
                                  ChatMessageId,
                                  ChatMessage,
                                  ChatType,
                                  MediaName,
                                  MediaSize,
                                  MediaData)
  end;
handle(?chatMessageDeliveredAt,FcmId,Json) ->
  FromUserId = maps:get(?userId,Json,<<>>),
  ToUserId = maps:get(?toUserId,Json,<<>>),
  ChatMessageId = maps:get(?chatMessageId,Json,<<>>),
  TimeStamp = data_utils:fetch_integer(?timeStamp,Json,-1),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when FromUserId =:= <<>>;ToUserId =:= <<>>;TaskId =:= <<>>;ChatMessageId =:= <<>>;TimeStamp =:= -1 ->
      ok;
    true ->
      functions:send_task_receipt(FcmId,TaskId),
      functions:chat_message_delivered_at(FromUserId,ToUserId,ChatMessageId,TimeStamp)
  end;
handle(?getS3ParamsForMedia,FcmId,Json) ->
  ChatMessageId = maps:get(?chatMessageId,Json,<<>>),
  FromUserId = maps:get(?userId,Json,<<>>),
  case true of
    true when FromUserId =:= <<>>;ChatMessageId =:= <<>> ->
      ok;
    true ->
      Response = functions:get_s3_params_for_media(FromUserId,ChatMessageId,FcmId),
      downstream:create(FcmId,Response)
  end;
handle(?messageDownloaded,FcmId,Json) ->
  ChatMessageId = maps:get(?chatMessageId,Json,<<>>),
  FromUserId = maps:get(?userId,Json,<<>>),
  TaskId = maps:get(?taskId,Json,<<>>),
  case true of
    true when FromUserId =:= <<>>;ChatMessageId =:= <<>>;TaskId =:= <<>> ->
      ok;
    true ->
      functions:send_task_receipt(FcmId,TaskId),
      aws_s3:delete_media_file(ChatMessageId)
  end;
handle(_Mtype,_FcmId,Json) ->
  applog:error(?MODULE,"Received Unhandled Message Type : ~p~n",[Json]).

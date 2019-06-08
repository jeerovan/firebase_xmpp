-module(functions).
-include("models.hrl").
-include("constants.hrl").

-export([send_fcm_to_all/1]).
-export([check_settings/0]).
-export([update_fcm_id/2,
         device_unregistered/1]).
-export([sign_up/4,
         sign_in/3]).
-export([update_age_gender_bio/4,
         update_dp_version/1,
         update_user_bio/2,
         update_last_seen_at/1,
         change_password/2]).
-export([set_security_questions/5,
         verify_security_questions/6]).
-export([get_s3_params_for_dp/2]).
-export([get_s3_params_for_media/3]).
-export([send_updated_profiles/2,
         get_pending_tasks/1,
         handle_task_receipt/2]).
-export([send_task_receipt/2]).
-export([send_general_notification/3,
         send_link_notification/4,
         send_update_notification/4]).
-export([fetch_last_seen_at/2,
         send_chat_message/8,
         chat_message_delivered_at/4]).
-export([get_active_user_profiles/2]).

check_settings() ->
  %------- Firebase Cloud Message (FCM) Settings --------
  _ = filesettings:get(fcm_sender_id,fcm_sender_id@gcm_dot_googleapis_dot_com_in_double_quotes),
  _ = filesettings:get(fcm_server_key,fcm_server_key_in_double_quotes),
  %------- AWS S3 Settings ------
  _ = filesettings:get(environment,define_environment_as_staging_or_production),
  _ = filesettings:get(s3_bucket,provide_s3_bucket_name_in_double_quotes),
  _ = filesettings:get(s3_access_id,provide_s3_access_id_in_double_quotes),
  _ = filesettings:get(s3_secret_key,provide_s3_secret_key_in_double_quotes),
  _ = filesettings:get(s3_region,provide_s3_region_in_double_quotes),
  _ = filesettings:get(s3_host,provide_s3_host_in_double_quotes).
send_fcm(FcmId,
         DataMap) ->
  downstream:create(FcmId,DataMap).
send_fcm_to_all(DataMap) ->
  [send_fcm(User#user.fcm_id,DataMap) || User <- db:read_table(user)].

get_user_map(User) ->
  {One,Two,Three,Four} = User#user.answers,
  #{?userName => User#user.username,
    ?userId => User#user.userid,
    ?country => User#user.country,
    ?userState => User#user.state,
    ?userAge => User#user.age,
    ?userGender => User#user.gender,
    ?dpVersion => User#user.dp_version,
    ?answerOne => One,
    ?answerTwo => Two,
    ?answerThree => Three,
    ?answerFour => Four,
    ?profileType => User#user.profile_type,
    ?userBio => User#user.bio,
    ?bioChangedAt => User#user.bio_changed_at}.
get_contact_map(User) ->
  #{?userName => User#user.username,
    ?userId => User#user.userid,
    ?country => User#user.country,
    ?userAge => User#user.age,
    ?userGender => User#user.gender,
    ?dpVersion => User#user.dp_version,
    ?profileType => User#user.profile_type,
    ?userBio => User#user.bio,
    ?bioChangedAt => User#user.bio_changed_at}.
sign_up(Username,
        Password,
        Country,
        FcmId) ->
  case db:record_exist(login,Username) of
    true ->
      #{ ?messageType => ?userSignUp,
         ?responseStatus => ?errorInResponse,
         ?errorField => ?userName,
         ?responseError => <<"Username Already Taken.">>};
    false ->
      UserIdInteger = dbsettings:get_next_counter(user_id_counter),
      Ctx = hashids:new([{salt,"tojeevansingh@gmail.com"}]),
      UserId = list_to_binary(hashids:encode(Ctx,UserIdInteger)),
      Login = #login{username = Username,
                     password = Password,
                     userid = UserId},
      User = #user{userid = UserId,
                   username = Username,
                   country = Country,
                   state = 1,
                   fcm_id = FcmId,
                   last_seen_at = time_util:utc_seconds()},
      db:write_record(Login),
      db:write_record(User),
      #{ ?messageType => ?userSignUp,
         ?responseStatus => ?successResponse,
         ?userId => UserId
       }
  end.
sign_in(Username,
        Password,
        FcmId) ->
  case db:read_record(login,Username) of
    [] ->
      #{?messageType => ?userSignIn,
        ?responseStatus => ?errorInResponse,
        ?errorField => ?userName,
        ?responseError => <<"No Such User">>};
    [Login] ->
      case Login#login.password =:= Password of
        false ->
          #{?messageType => ?userSignIn,
            ?responseStatus => ?errorInResponse,
            ?errorField => ?passWord,
            ?responseError => <<"Incorrect Password">>};
        true ->
          UserId = Login#login.userid,
          case db:read_record(user,UserId) of
            [] ->
              applog:error(?MODULE,"Login Successful But No User Entry~n",[]),
              #{?messageType => ?userSignIn,
                ?responseStatus => ?errorInResponse,
                ?errorField => ?generalError,
                ?responseError => <<"No Such User">>};
            [User] ->
              db:write_record(User#user{fcm_id = FcmId}),
              maps:merge(#{?messageType => ?userSignIn,
                           ?responseStatus => ?successResponse
                          },
                          get_user_map(User))
          end
      end
  end.
set_security_questions(UserId,
                       One,
                       Two,
                       Three,
                       Four) ->
  case db:read_record(user,UserId) of
    [] ->
      ok;
    [User] ->
      NewState =
        case User#user.state < 2 of
          true ->
            2;
          false ->
            User#user.state
        end,
      db:write_record(User#user{answers = {One,Two,Three,Four},
                                state = NewState})
  end.
change_password(UserId,
                NewPassword) ->
  case db:read_record(user,UserId) of
    [] ->
      ok;
    [User] ->
      [Login] = db:read_record(login,User#user.username),
      db:write_record(Login#login{password = NewPassword})
  end.
verify_security_questions(FcmId,
                          Username,
                          One,
                          Two,
                          Three,
                          Four) ->
  case db:read_record(login,Username) of
    [] ->
      #{?messageType => ?verifySecurityAnswers,
        ?responseStatus => ?errorInResponse,
        ?responseError => <<"No Such User">>};
    [Login] ->
      [User] = db:read_record(user,Login#login.userid),
      case User#user.answers =:= {One,Two,Three,Four} of
        false ->
          #{?messageType => ?verifySecurityAnswers,
            ?responseStatus => ?errorInResponse,
            ?responseError => <<"Details Did NOT Match.">>};
        true ->
          case User#user.fcm_id of
            <<>> ->
              db:write_record(User#user{fcm_id = FcmId});
            OldFcmId ->
              case OldFcmId =:= FcmId of
                true ->
                  ok;
                false ->
                  fcm_manager:update_registration_id(OldFcmId,FcmId)
              end
          end,
          maps:merge(#{?messageType => ?verifySecurityAnswers,
                       ?responseStatus => ?successResponse
                      },
                      get_user_map(User))
      end
  end.
%--------- S3 PARAMS --------
get_s3_params_for_dp(UserId,FcmId) ->
  case db:read_record(user,UserId) of
    [] ->
      #{?messageType => ?getS3ParamsForDp,
        ?responseStatus => ?errorInResponse,
        ?responseError => <<"No Such User">>};
    [User] ->
      case User#user.fcm_id =:= FcmId of
        true ->
          maps:merge(#{?messageType => ?getS3ParamsForDp,
                       ?responseStatus => ?successResponse},
                       aws_s3:get_s3_post_params(UserId));
        false ->
          #{?messageType => ?getS3ParamsForDp,
            ?responseStatus => ?errorInResponse,
            ?responseError => <<"Details Mismatch">>}
      end
  end.
get_s3_params_for_media(UserId,
                        MessageId,
                        FcmId) ->
  case db:read_record(user,UserId) of
    [] ->
      #{?messageType => ?getS3ParamsForMedia,
        ?responseStatus => ?errorInResponse,
        ?responseError => <<"No Such User">>};
    [User] ->
      case User#user.fcm_id =:= FcmId of
        true ->
          #{?messageType => ?getS3ParamsForMedia,
            ?responseStatus => ?successResponse,
            ?timeStamp => time_util:utc_seconds(),
            ?chatMessageId => MessageId,
            ?mediaS3Params => aws_s3:get_s3_post_params(MessageId)};
        false ->
          #{?messageType => ?getS3ParamsForMedia,
            ?responseStatus => ?errorInResponse,
            ?responseError => <<"Details Mismatch">>}
      end
  end.
%-------- Update FCM Id ------
update_fcm_id(OldFcmId,
              NewFcmId) ->
  case db:match_object(user,
                       #user{fcm_id = OldFcmId,
                             _ = '_'}) of
    [] ->
      ok;
    Users ->
      [db:write_record(User#user{fcm_id = NewFcmId}) || User <- Users]
  end.
device_unregistered(FcmId) ->
  case db:match_object(user,
                       #user{fcm_id = FcmId,
                             _ = '_'}) of
    [] ->
      ok;
    Users ->
      [db:write_record(User#user{fcm_id = <<>>}) || User <- Users]
  end.
%-------- User Age,Gender,DpVersion,Bio ----
update_age_gender_bio(UserId,
                      Age,
                      Gender,
                      Bio) ->
  case db:read_record(user,UserId) of
    [] ->
      #{?messageType => ?updateAgeGenderBio,
        ?responseStatus => ?errorInResponse,
        ?responseError => <<"No Such User">>};
    [User] ->
      NewState =
        case User#user.state < 3 of
          true ->
            3;
          false ->
            User#user.state
        end,
      db:write_record(User#user{age = Age,
                                gender = Gender,
                                bio = Bio,
                                bio_changed_at = time_util:utc_seconds(),
                                state = NewState}),
      #{?messageType => ?updateAgeGenderBio,
        ?responseStatus => ?successResponse,
        ?userState => NewState}
  end.
update_dp_version(UserId) ->
  case db:read_record(user,UserId) of
    [] ->
      #{?messageType => ?updateDpVersion,
        ?responseStatus => ?errorInResponse,
        ?responseError => <<"No Such User">>};
    [User] ->
      db:write_record(User#user{dp_version = User#user.dp_version + 1,
                                bio_changed_at = time_util:utc_seconds()}),
      #{?messageType => ?updateDpVersion,
        ?responseStatus => ?successResponse}
  end.
update_user_bio(UserId,Bio) ->
  case db:read_record(user,UserId) of
    [] ->
      #{?messageType => ?updateUserBio,
        ?responseStatus => ?errorInResponse,
        ?responseError => <<"No Such User">>};
    [User] ->
      db:write_record(User#user{bio = Bio,
                                bio_changed_at = time_util:utc_seconds()}),
      #{?messageType => ?updateUserBio,
        ?responseStatus => ?successResponse}
  end.
update_last_seen_at(UserId) ->
  case db:read_record(user,UserId) of
    [] ->
      ok;
    [User] ->
      db:write_record(User#user{last_seen_at = time_util:utc_seconds()})
  end.
send_updated_profiles(FcmId,ProfilesData) ->
  Items = binary:split(ProfilesData,<<",">>,[global]),
  send_updated_profiles(Items,[],FcmId).
send_updated_profiles([],[],_FcmId) ->
  ok;
send_updated_profiles([],Acc,FcmId) ->
  downstream:create(FcmId,#{?messageType => ?updateProfiles,
                            ?profilesData => Acc});
send_updated_profiles([Item|Items],Acc,FcmId) ->
  [UserId,LuaBin] = binary:split(Item,<<"|">>,[global]),
  Lua = binary_to_integer(LuaBin),
  UpdatedAcc =
    case db:read_record(user,UserId) of
      [] ->
        Acc;
      [User] ->
        case User#user.bio_changed_at > Lua of
          true ->
            [#{?userId => UserId,
              ?dpVersion => User#user.dp_version,
              ?userBio => User#user.bio,
              ?bioChangedAt => User#user.bio_changed_at} | Acc];
          false ->
            Acc
        end
    end, 
  {NewItems,NewAcc} =
    case byte_size(jsx:encode(Acc)) > 3950 of
      true ->
        downstream:create(FcmId,
                          #{?messageType => ?updateProfiles,
                            ?profilesData => Acc}),
        {[Item|Items],[]};
      false ->
        {Items,UpdatedAcc}
    end,
  send_updated_profiles(NewItems,NewAcc,FcmId).
get_active_user_profiles(UserId,FcmId) ->
  Now = time_util:utc_seconds(),
  ActiveForLastSeconds = filesettings:get(user_active_duration_seconds,3600),
  ActiveDuration = Now - ActiveForLastSeconds,
  F = fun() ->
        MatchHead = #user{last_seen_at = '$1',userid = '$2',_ = '_'},
        Guard = [{'>','$1',ActiveDuration},{'=/=','$2',UserId}],
        Result = '$_',
        mnesia:select(user,[{MatchHead,Guard,[Result]}],100,read)
      end,
  Result = mnesia:activity(sync_transaction,F),
  {Profiles,_Cont} = if Result =:= '$end_of_table' ->
                       {[],empty};
                      true ->
                        Result
                    end,
  LsaProfiles = [{Profile#user.last_seen_at,Profile} || Profile <- Profiles],
  SortedProfiles = lists:reverse(lists:sort(LsaProfiles)),
  NumberOfActiveProfilesToSend = filesettings:get(number_of_active_profiles_to_send,10),
  ProfilesToSend =
    case length(SortedProfiles) > NumberOfActiveProfilesToSend of
      true ->
        {DesiredProfiles,_RestProfiles} = lists:split(NumberOfActiveProfilesToSend,SortedProfiles),
        DesiredProfiles;
      false ->
        SortedProfiles
    end,
  ProfilesData = [get_contact_map(UserProfile) || {_Lsa,UserProfile} <- ProfilesToSend], 
  downstream:create(FcmId,#{?messageType => ?getActiveUserProfiles,
                            ?responseStatus => ?successResponse,
                            ?profilesData => ProfilesData}).

get_pending_tasks(UserId) ->
  case db:read_record(user,UserId) of
    [] ->
      ok;
    [User] ->
      case db:match_object(task,#task{to = UserId,_ = '_'}) of
        [] ->
          ok;
        Rows ->
          TimeTasks = [{Row#task.added_at,Row#task.data} || Row <- Rows],
          SortedTasks = lists:sort(TimeTasks),
          Tasks = [Task || {_Time,Task} <- SortedTasks],
          Response = #{?messageType => ?pendingTasks,
                       ?responseStatus => ?successResponse,
                       ?tasks => Tasks},
          % TODO If Message Grows Beyond 4KB
          downstream:create(User#user.fcm_id,Response)
      end
  end.
handle_task_receipt(UserId,TaskId) ->
  case db:read_record(task,TaskId) of
    [] ->
      ok;
    [Task] ->
      case Task#task.to =:= UserId of
        true ->
          db:delete_object(Task);
        false ->
          ok
      end
  end.
send_task_receipt(FcmId,TaskId) ->
  Receipt = #{?messageType => ?taskReceipt,
              ?responseStatus => ?successResponse,
              ?taskId => TaskId},
  downstream:create(FcmId,Receipt).
%---------- FCM Notifications -------------
send_general_notification(FcmId,Title,Message) ->
  Notification = #{?messageType => ?fcmGeneralNotification,
                   ?responseStatus => ?successResponse,
                   is_notification => true,
                   ?fcmNotificationTitle => Title,
                   ?fcmNotificationMessage => Message},
  downstream:create(FcmId,Notification).
send_link_notification(FcmId,Title,Message,Url) ->
  Notification = #{?messageType => ?fcmGeneralNotification,
                   ?responseStatus => ?successResponse,
                   ?fcmNotificationTitle => Title,
                   ?fcmNotificationMessage => Message,
                   ?fcmNotificationUrl => Url},
  downstream:create(FcmId,Notification).
send_update_notification(FcmId,Title,Message,Version) ->
  Notification = #{?messageType => ?fcmGeneralNotification,
                   ?responseStatus => ?successResponse,
                   ?fcmNotificationTitle => Title,
                   ?fcmNotificationMessage => Message,
                   ?fcmNotificationVersion => Version},
  downstream:create(FcmId,Notification).
%------- Chats --------
fetch_last_seen_at(FromUserId,UserId) ->
  case db:read_record(user,FromUserId) of
    [] ->
      ok;
    [FromUser] ->
      case db:read_record(user,UserId) of
        [] ->
          ok;
        [User] ->
          Response = #{?messageType => ?fetchLastSeenAt,
                       ?responseStatus => ?successResponse,
                       ?lastSeenAt => User#user.last_seen_at},
          downstream:create(FromUser#user.fcm_id,Response)
      end
  end.
send_chat_message(FromUserId,
                  ToUserId,
                  ChatMessageId,
                  ChatMessage,
                  ChatType,
                  MediaName,
                  MediaSize,
                  MediaData) ->
  case db:read_record(user,FromUserId) of
    [] ->
      ok;
    _ ->
      case db:read_record(user,ToUserId) of
        [] ->
          ok;
        [ToUser] ->
          Now = time_util:utc_seconds(),
          %--- Deliver Chat Message ---
          MessageTypeBin = integer_to_binary(?deliverChatMessage),
          DeliverTaskId = <<ChatMessageId/binary,MessageTypeBin/binary>>,
          case db:read_record(task,DeliverTaskId) of
            [] ->
              Delivery = #{?messageType => ?deliverChatMessage,
                           ?responseStatus => ?successResponse,
                           ?chatMessageId => ChatMessageId,
                           ?chatMessage => ChatMessage,
                           ?chatMessageType => ChatType,
                           ?mediaName => MediaName,
                           ?mediaSize => MediaSize,
                           ?mediaData => MediaData,
                           ?userId => FromUserId,
                           ?taskId => DeliverTaskId,
                           priority => high},
              db:write_record(#task{id = DeliverTaskId,
                                    from = FromUserId,
                                    to = ToUserId,
                                    type = ?deliverChatMessage,
                                    data = Delivery, 
                                    added_at = Now}),
              downstream:create(ToUser#user.fcm_id,Delivery);
            _ ->
              ok
          end
      end
  end.
chat_message_delivered_at(FromUserId,ToUserId,ChatMessageId,TimeStamp) ->
  case db:read_record(user,FromUserId) of
    [] ->
      ok;
    _ ->
      case db:read_record(user,ToUserId) of
        [] ->
          ok;
        [ToUser] ->
          Now = time_util:utc_seconds(),
          MessageTypeBin = integer_to_binary(?chatMessageDeliveredAt),
          TaskId = <<ChatMessageId/binary,MessageTypeBin/binary>>,
          case db:read_record(task,TaskId) of
            [] ->
              DeliveredAt = #{?messageType => ?chatMessageDeliveredAt,
                              ?responseStatus => ?successResponse,
                              ?chatMessageId => ChatMessageId,
                              ?timeStamp => TimeStamp,
                              ?taskId => TaskId},
              db:write_record(#task{id = TaskId,
                                    to = ToUserId,
                                    from = FromUserId,
                                    type = ?chatMessageDeliveredAt,
                                    data = DeliveredAt,
                                    added_at = Now}),
              downstream:create(ToUser#user.fcm_id,DeliveredAt);
            _ ->
              ok
          end
      end
  end.

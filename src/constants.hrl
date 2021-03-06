%------- Message Types -----
-define(getAppSettings,0).
-define(userSignUp,1).
-define(userSignIn,2).
-define(setSecurityAnswers,3).
-define(changePassword,4).
-define(verifySecurityAnswers,5).
-define(getS3ParamsForDp,6).
-define(updateFcmId,7).
-define(updateAgeGenderBio,8).
-define(updateDpVersion,9).
-define(updateUserBio,10).
-define(updateProfiles,11).
-define(pendingTasks,14).
-define(taskReceipt,15).
-define(fcmGeneralNotification,18).
-define(fcmLinkNotification,19).
-define(fcmUpdateNotification,20).
-define(fetchLastSeenAt,27).
-define(sendChatMessage,28).
-define(chatMessageSentAt,29).
-define(deliverChatMessage,30).
-define(chatMessageDeliveredAt,31).
-define(getS3ParamsForMedia,32).
-define(getActiveUserProfiles,33).
-define(messageDownloaded,34).
%------ Message Keys -----
-define(fcmId,<<"a">>).
-define(userId,<<"b">>).
-define(generalRequestTimeoutMillis,<<"c">>).
-define(messageType,<<"d">>).
-define(userName,<<"e">>).
-define(passWord,<<"f">>).
-define(country,<<"g">>).
-define(answerOne,<<"h">>).
-define(answerTwo,<<"i">>).
-define(answerThree,<<"j">>).
-define(answerFour,<<"k">>).
-define(oldFcmId,<<"l">>).
-define(newFcmId,<<"m">>).
-define(userAge,<<"n">>).
-define(userGender,<<"o">>).
-define(userBio,<<"p">>).
-define(securityState,<<"q">>).
-define(verifyingSecurity,<<"r">>).
-define(errorField,<<"s">>).
-define(responseStatus,<<"t">>).
-define(responseError,<<"u">>).
-define(atAnswer,<<"v">>).
-define(termsAccepted,<<"w">>).
-define(userState,<<"x">>).
-define(settingSecurity,<<"y">>).
-define(dpVersion,<<"z">>).
-define(s3Bucket,<<"aa">>).
-define(s3Key,<<"ab">>).
-define(s3Acl,<<"ac">>).
-define(s3MetaUuid,<<"ad">>).
-define(s3Policy,<<"ae">>).
-define(s3Credential,<<"af">>).
-define(s3Algorithm,<<"ag">>).
-define(s3Signature,<<"ah">>).
-define(s3Date,<<"ai">>).
-define(s3Url,<<"aj">>).
-define(generalError,<<"ak">>).
-define(profileType,<<"al">>).
-define(fcmNotificationTitle,<<"am">>).
-define(fcmNotificationMessage,<<"an">>).
-define(fcmNotificationVersion,<<"ao">>).
-define(fcmNotificationUrl,<<"ap">>).
-define(tasks,<<"aq">>).
-define(taskId,<<"ar">>).
-define(bioChangedAt,<<"as">>).
-define(toUserId,<<"at">>).
-define(profilesData,<<"bm">>).
-define(lastSeenAt,<<"bn">>).
-define(fcmNotificationPackageId,<<"bo">>).
-define(chatMessage,<<"bp">>).
-define(chatMessageId,<<"bq">>).
-define(chatMessageType,<<"br">>).
-define(timeStamp,<<"bs">>).
-define(mediaS3Params,<<"bt">>).
-define(s3ParamsExpirySeconds,<<"bu">>).
-define(mediaName,<<"bv">>).
-define(mediaSize,<<"bw">>).
-define(mediaData,<<"bx">>).
%----- Message Values ------
-define(errorInResponse,0).
-define(successResponse,1).
-define(parametersMissing,<<"Parameters Missing">>).

# Backend for Android App (Fatalk)

## What is it about?
- This Repo Is A Backend Implementation For A Demo Chat Android Application, Showing Integration With Firebase Cloud Messaging Service
- It Also Includes Using AWS S3, To Upload Media Files From Client Side

## Links
- **Android App (Fatalk) ->** [https://play.google.com/store/apps/details?id=com.kaarss.fatalk](https://play.google.com/store/apps/details?id=com.kaarss.fatalk)
- **Android App Repo ->** [https://github.com/jeerovan/fatalk](https://github.com/jeerovan/fatalk)
- **Firebase Cloud Messaging Service ->** [https://github.com/jeerovan/firebase_cloud_messaging](https://github.com/jeerovan/firebase_cloud_messaging)

## Setting Up
- Install Erlang (Search Google)
- Clone Repo
- Execute Command `make` At Root Folder Location. This Will Download Required Dependencies.
- To Run In A Console, Execute `_rel/firebase_xmpp_release/bin/firebase_xmpp_release console`
- Execute Following Commands To Initialize Database And Settings
```
mnesia:stop().
mnesia:create_schema([node()]).
mnesia:start().
db:init().
```
- Execute Command `q().` To Quit For Now.
- You'll Have A Settings File Named : `settings.txt`. It Contains All The Settings You Can Configure To Communicate With AWS & Firebase Cloud Messaging Service
- If The `fcm_connection_limit` Is Set To 0 In `settings.txt` You May Initialize A New FCM Connection With Following Command: `fcm_manager:create_fcm_process().`
- If AWS S3 Access Is Required, Provide Necessary Parameters In `settings.txt` And Configure Your Bucket To Have Public Access.
- Set Log Levels `verbose/info/debug/error` To Control Logs In `settings.txt`
- After Setting Up Parameters, Execute Console Command To Run

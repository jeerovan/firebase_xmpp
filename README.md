# Firebase Cloud Messaging With XMPP Servers

## What is it about?
- Erlang Implementation Of Firebase Cloud Messaging Through XMPP Servers
- This Repo Is A Backend Implementation For A Demo Chat Android Application Using FCMs
- It Also Includes Using AWS S3, To Upload Media Files From Client Side

## Links
- **Android App (Fatalk) ->** [https://play.google.com/store/apps/details?id=com.kaarss.fatalk](https://play.google.com/store/apps/details?id=com.kaarss.fatalk)
- **Android App Repo ->** [https://github.com/jeerovan/fatalk](https://github.com/jeerovan/fatalk)

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
- You'll Have A Settings File Named : `settings.txt`. It Contains All The Settings You Can Configure To Establish Firebase XMPP Connections And Set Different Limits As Required.
- To Establish Firebase Connections, `fcm_sender_id` & `fcm_server_key` Are Required. These You Can Find In Your Firebase Project's Cloud Messaging Section
- If The `fcm_connection_limit` Is Set To 0 In `settings.txt` You May Initialize A New FCM Connection With Following Command: `fcm_manager:create_fcm_process().`
- If AWS S3 Access Is Required, Provide Necessary Parameters In `settings.txt` And Configure Your Bucket To Have Public Access.
- Set Log Levels `verbose/info/debug/error` To Control Logs In `settings.txt`
- After Setting Up Parameters, Execute Console Command To Run And Create FCM Connections

## Features And Support
- Supports Only Android For Now (No Support For iOS)
- Sends Only `data` Notifications With Priority (high/normal) Having TTL 0 To Improve Latency
- Sends Bundle Of Messages To Optimize Bandwidth
- Does Not Support Delivery Receipt
- Automatic Retry Of `nack` Messages
- Configurable Paramters To Control Throughput And Message Rate Per Device
- Handles Connections With State `idle` Or `service unavailable`
- Access Monitor Panel At `http://localhost:33000/stats`


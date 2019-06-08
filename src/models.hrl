%--------- User ---------
-record(login,{username :: binary(),
               password :: binary(),
               userid :: binary()
              }
       ).
-record(user,{userid :: binary(),
              username :: binary(),
              country :: binary(),
              state = 0 :: integer(), % 1:Set Security,2:Set Profile,3:Main
              age = 0 :: integer(),
              gender = 1 :: integer(), % 0:female,1:male
              dp_version = 0 :: integer(),
              answers = {<<>>,<<>>,<<>>,<<>>} :: tuple(),
              bio = <<>> :: binary(),
              bio_changed_at = time_util:utc_seconds() :: integer(),
              last_seen_at = 0 :: integer(),
              fcm_id = <<>> :: binary(),
              profile_type = 1 :: integer() % 1:private;0:public
             }
       ).
-record(dbsetting,{name :: atom(),
                   value :: any()}
       ).
-record(task,{id :: binary(),
              from :: binary(),
              to :: binary(),
              type :: integer(),
              data :: any(),
              added_at :: integer()
             }
       ).

-module(db).
-include("models.hrl").

%------ Writes Should Always Go Through Mnesia, Cause It Keeps Write Requests Saved In Case The Mensia Process Dies, It Will Write Pending Requests
%------ Read Should Always Be From ETS, It Doesn't Create Extra Overhead Of Checking.

%% API.
-export([read_table/1,
         read_record/2,
         record_exist/2,
         match_object/2]).
-export([init/0,
         reset/0,
         delete_tables/1,
         delete_record/2,
         delete_object/1,
         truncate_tables/1,
         write_record/1,
         match_delete/1]).

-define(TABLES,[
                {dbsetting,disc_copies,set},
                {login,disc_copies,set},
                {task,disc_copies,set},
                {user,disc_copies,set}
              ]).
record_fields(dbsetting)      -> record_info(fields,dbsetting);
record_fields(login)          -> record_info(fields,login);
record_fields(task)           -> record_info(fields,task);
record_fields(user)           -> record_info(fields,user).

%------- DIRECT READ APIS -------
read_table(Table) ->
  ets:tab2list(Table).
read_record(Table,Key) ->
  ets:lookup(Table,Key).
record_exist(Table,Key) ->
  ets:member(Table,Key).
match_object(Table,Pattern) ->
  ets:match_object(Table,Pattern).

%------- DIRECT WRITE/UPDATE APIS -------
init() ->
  [create_table(Table,StorageType,Type) || {Table,StorageType,Type} <- ?TABLES].
reset() ->
  [mnesia:delete_table(Table) || {Table,_StorageType,_Type} <- ?TABLES],
  [create_table(Table,StorageType,Type) || {Table,StorageType,Type} <- ?TABLES].
delete_tables(Tables) ->
  [mnesia:delete_table(Table) || Table <- Tables].
truncate_tables(Tables) ->
  [mnesia:clear_table(Table) || Table <- Tables].
write_record(Record) ->
  mnesia:activity(sync_transaction,fun() -> mnesia:write(Record) end).
delete_record(Table,Key) ->
  mnesia:activity(sync_transaction,fun() -> mnesia:delete({Table,Key}) end).
delete_object(Record) ->
  mnesia:activity(sync_transaction,fun() -> mnesia:delete_object(Record) end).
match_delete(Pattern) ->
  Fun = fun() ->
          List = mnesia:match_object(Pattern),
          lists:foreach(fun(X) ->
                              mnesia:delete_object(X)
                        end, List)
        end,
  mnesia:activity(sync_transaction,Fun).

%--------------- FUNCTIONS -------------
create_table(Table,StorageType,TableType) ->
  try
      mnesia:table_info(Table, type)
  catch
      exit:{aborted, {no_exists, Table, type}} ->
           {atomic, ok} = mnesia:create_table(Table,
              [{attributes, record_fields(Table)},
              {type, TableType},
              {StorageType, [node()]}])
  end.
